let check_pattern source (pattern : _ Pattern.matcher) =
  let ast = Pyast.Latest.parse source in
  match
    pattern ~quoted:(Refl.Lift.Exp.lift [%refl: Pyast.Latest.mod_] [] ast) ast
  with
  | Ok _bindings -> ()
  | Error failure ->
     Alcotest.fail
       (Format.asprintf "@[failed:@ %a@]@." Pattern.format_failure failure)

let test_pattern source pattern =
  source, `Quick, fun () -> check_pattern source pattern

let () = Py.initialize ()

let () =
  Alcotest.run "simple" [
    "simple", [
      test_pattern {|
x = 18
      |} [%pattern?
        Module { body = [{
         desc = Assign {
           targets = [{
             desc = Name { id = "x"; ctx = Store }; _}];
           value = { desc = Constant {
             value = Some (Num (Int 18));
             kind = None }; _ }; _ }; _ }]; _ }];
      test_pattern {|
def f(x):
  print(x)
      |} [%pattern?
        Module { body = [{
          desc = FunctionDef {
            name = "f";
            args = { args = [{ arg = "x"; _ }]; _ };
            body = [{
              desc = Expr { desc = Call {
                func = { desc = Name { id = "print"; _ }; _ };
                args = [{ desc = Name { id = "x"; _ }; _ }];
                _ }; _ }; _ }]; _ }; _ }]; _ }]
]]
