let code1 = {|/* Inline evaluation and stdout */
print_endline("Hello ReasonML folks!");
type say = | Hello | Bye;

/* A warning */
fun
| Hello
| Bye => ()
| _ => ();

/* A syntax error */
a|};

let code3 = {|type say = | Hello | Bye;
fun
| Hello => ()
| Bye => ()
| _ => ();|};

let code2 = {|type tree = Leaf | Node(int, tree, tree);

let rec sum = (item) => {
  switch (item) {
  | Leaf => 0
  | Node(value, left, right) => value + sum(left) + sum(right);
  }
};

let myTree =
  Node(
    1,
    Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)),
    Node(3, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf))
  );

Printf.sprintf("%i", sum(myTree));|};

let text1 = {|You're viewing an early demo of ReasonML playground.

A note writing application for teaching and sharing ReasonML code with inline evaluation.

This is **OCaml 4.06.0 toplevel** compiled with [js_of_ocaml](http://ocsigen.org/js_of_ocaml/).

Please note that this is **not** Bucklescript so you can't use these modules:
  - Js
  - Dom
  - Node
  - Belt (Belt could be compiled with js_of_ocaml so this could be done with some works)

## Usage:

- Write your code
- Press **Shift-enter** to evaluate each codeblock
- Share your work

## Contribute

This is an open source project, if you see a missing feature, you can help me build it

Repository: https://github.com/thangngoc89/rtop-ui|};
