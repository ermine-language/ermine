module Error where

private

-- native error : String -> a

undefined : a
undefined = error "undefined"

todo : a
todo = error "todo"

todoWith: String -> a
todoWith s = error s
