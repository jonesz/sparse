import "symmetric"

module sparse = mk_symmetric {open i32 def conj x = x }

-- ==
-- entry: test_smvm
-- input { [1i32, 3, 6, 2, 9, 1] [6i32, 7, 8] }
-- output { [43i32, 132, 83] }

entry test_smvm A x =
	sparse.symmetric A |> flip sparse.smvm x
