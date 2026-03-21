module ListExtraExtra exposing (insertAt)


insertAt : Int -> a -> List a -> List a
insertAt i x list =
    List.take i list ++ (x :: List.drop i list)
