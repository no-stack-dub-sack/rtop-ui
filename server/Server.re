open! Express;

let isProduction = Utils.env == "production";

let app = Express.App.make();

let port = 3001;

App.use(app, Morgan.(make(isProduction ? Combined : Dev)));

App.get(
  app,
  ~path="/healthz",
  Middleware.from((_, _) => Response.sendString("ok")),
);

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
