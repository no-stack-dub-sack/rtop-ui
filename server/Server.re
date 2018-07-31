open! Express;

let isProduction = false;

[@bs.module] external morgan : string => Express.Middleware.t = "";

let app = Express.App.make();

let port = 3001;

App.use(app, morgan(isProduction ? "combined" : "dev"));

App.get(
  app,
  ~path="/healthz",
  Middleware.from((_, _) => Response.sendString("ok")),
);

let api = {
  let api = Express.router();

  Router.get(api, ~path="/search") @@
  PromiseMiddleware.from((next, req, res) =>
    switch (Utils.getDictString(Request.query(req), "query")) {
    | Some(query) =>
      let query = query |> Js.String.toLowerCase |> Js.String.trim;
      Js.Promise.(
        Service_AutoComplete.query(~limit=20, query)
        |> then_(arrayOfSuggestions =>
             arrayOfSuggestions
             |. Json.Encode.jsonArray
             |. Response.sendJson(res)
             |. Js.Promise.resolve
           )
        /* TODO: send a well defined error message */
        |> catch(_a => Js.Promise.resolve(Response.sendString("no", res)))
      );
    | _ => next(Next.route, res) |> Js.Promise.resolve
    }
  );

  Router.get(api, ~path="/word/:word") @@
  PromiseMiddleware.from((next, req, res) =>
    switch (Utils.getDictString(Request.params(req), "word")) {
    | Some(requestedWord) =>
      let requestedWord = Js.String.trim(requestedWord);
      Js.Promise.(
        Database.Main.get(requestedWord)
        |> then_(result =>
             switch (result) {
             | Belt.Result.Ok(founded) =>
               switch (founded) {
               | Some(founded) =>
                 founded |. Response.sendJson(res) |. resolve
               | None => Response.sendStatus(NotFound, res) |. resolve
               }
             | Error(error) =>
               /* TODO: handle error */
               Js.log(error);
               Response.sendStatus(InternalServerError, res) |. resolve;
             }
           )
      );

    | _ => next(Next.route, res) |> Js.Promise.resolve
    }
  );
  api;
};

App.useRouterOnPath(app, ~path="/api/v1", api);

App.get(
  app,
  ~path="/",
  Middleware.from((_, _) => Response.sendString("Hello world")),
);

let onListen = e =>
  switch (e) {
  | exception (Js.Exn.Error(e)) =>
    Js.log(e);
    Node.Process.exit(1);
  | _ => Js.log @@ "Listening at http://localhost:3001"
  };

let server = App.listen(app, ~port, ~onListen, ());
