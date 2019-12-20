package main

import "fmt"

func Solve1(program string, initialInput int) int {
	max := -1
	for a := 0; a < 5; a++ {
		for b := 0; b < 5; b++ {
			for c := 0; c < 5; c++ {
				for d := 0; d < 5; d++ {
					for e := 0; e < 5; e++ {
						if a != b && a != c && a != d && a != e && b != c && b != d && b != e && c != d && c != e && d != e {
							op1 := IntCode(program, []int{a, initialInput})
							op2 := IntCode(program, []int{b, op1})
							op3 := IntCode(program, []int{c, op2})
							op4 := IntCode(program, []int{d, op3})
							op5 := IntCode(program, []int{e, op4})
							if op5 > max {
								max = op5
							}
						}
					}
				}
			}
		}
	}
	fmt.Println(max)
	return max
}
