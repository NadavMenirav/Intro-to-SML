fun insert_at x n lst =
    let
      fun aux x 0 [] = [x]
        | aux x i (h :: t) =
            if i = 0 then x :: h :: t
            else h :: aux x (i-1) t
    in
      aux x n lst
    end;