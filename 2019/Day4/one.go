package main

import "fmt"

func isIncreasing(n []int) bool {
	lenN := len(n)
	for i := range n {
		if i == lenN-1 {
			continue
		}
		if n[i] > n[i+1] {
			return false
		}
	}
	return true
}

func double(n []int) bool {
	lenN := len(n)
	for i := range n {
		if i == lenN-1 {
			continue
		}
		if n[i] == n[i+1] {
			return true
		}
	}
	return false
}

func digits(n int) []int {
	var d []int
	for n > 0 {
		d = append(d, n%10)
		n /= 10
	}
	lenD := len(d)
	for i := lenD/2 - 1; i >= 0; i-- {
		o := lenD - i - 1
		d[i], d[o] = d[o], d[i]
	}
	return d
}

func Solve1(start, end int) ([]int, int) {
	var solutionSpace []int
	for s := start; s <= end; s++ {
		d := digits(s)
		if isIncreasing(d) && double(d) {
			solutionSpace = append(solutionSpace, s)

		}
	}
	fmt.Println("Problem 1: ", len(solutionSpace))
	return solutionSpace, len(solutionSpace)
}

// Answer: 1653
