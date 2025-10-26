while true do
    print_string "> ";
    let expr = read_line () in
    if not (String.for_all ((=) ' ') expr) then
        try
            print_float (Calculator.eval_string expr);
            print_newline ();
        with
        | e -> Format.printf "Evaluation failed: %s\n" (Printexc.to_string e)
done
