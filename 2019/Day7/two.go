package main

import "fmt"

func Solve2(program string, initialInput int) int {
	max := -1
	for a := 5; a < 10; a++ {
		for b := 5; b < 10; b++ {
			for c := 5; c < 10; c++ {
				for d := 5; d < 10; d++ {
					for e := 5; e < 10; e++ {
						if a != b && a != c && a != d && a != e && b != c && b != d && b != e && c != d && c != e && d != e {
							var l1, l2, l3, l4, l5 int
							var op1, op2, op3, op4 int
							var io1, io2, io3, io4, io5 int
							var top1, top2, top3, top4, top5 int
							var e1, e2, e3, e4, e5 int
							p1, p2, p3, p4, p5 := parseInput(program), parseInput(program), parseInput(program), parseInput(program), parseInput(program)
							for {
								top1, l1, p1, e1, io1 = IntCode2(p1, []int{a, op1}, l1, io1)
								if e1 != 0 {
									op1 = top1
								}
								top2, l2, p2, e2, io2 = IntCode2(p2, []int{b, op1}, l2, io2)
								if e2 != 0 {
									op2 = top2
								}
								top3, l3, p3, e3, io3 = IntCode2(p3, []int{c, op2}, l3, io3)
								if e3 != 0 {
									op3 = top3
								}
								top4, l4, p4, e4, io4 = IntCode2(p4, []int{d, op3}, l4, io4)
								if e4 != 0 {
									op4 = top4
								}
								top5, l5, p5, e5, io5 = IntCode2(p5, []int{e, op4}, l5, io5)
								if e5 != 0 {
									op1 = top5
								}
								if e5 == 0 {
									break
								}
							}
							if op1 > max {
								max = op1
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
