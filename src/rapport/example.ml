let answer_to_any_question question = 
  match question with
  | 6 -> prInt 6
  | _ -> prInt 42
in 
let question = "to be fouine or not to be ?"
in answer_to_any_question question
