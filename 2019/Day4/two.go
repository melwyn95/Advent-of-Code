package main

import "fmt"

func doubleEven(n []int) bool {
	lenN := len(n)
	m := map[int]int{}
	for i := range n {
		if i == lenN-1 {
			continue
		}
		if n[i] == n[i+1] {
			_, ok := m[n[i]]
			if ok {
				m[n[i]]++
			} else {
				m[n[i]] = 1
			}
		}
	}
	for _, v := range m {
		if v == 1 {
			return true
		}
	}
	return false
}

func Solve2(start, end int) ([]int, int) {
	var solutionSpace []int
	for s := start; s <= end; s++ {
		d := digits(s)
		if isIncreasing(d) && doubleEven(d) {
			solutionSpace = append(solutionSpace, s)
		}

	}
	fmt.Println("Problem 1: ", len(solutionSpace))
	return solutionSpace, len(solutionSpace)
}
