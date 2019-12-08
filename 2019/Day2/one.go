package main

import (
	"fmt"
	"strconv"
	"strings"
)

func parseInput(s string) []int {
	xs := strings.Split(s, ",")
	iXs := make([]int, len(xs))
	for i := range xs {
		v, _ := strconv.Atoi(xs[i])
		iXs[i] = v
	}
	return iXs
}

func Solve1(input string) int {
	xs := parseInput(input)
	L := len(xs)
	for i := 0; i < L; {
		if xs[i] == 99 {
			i++
			break
		} else if xs[i] == 1 {
			xs[xs[i+3]] = xs[xs[i+1]] + xs[xs[i+2]]
			i += 4
		} else if xs[i] == 2 {
			xs[xs[i+3]] = xs[xs[i+1]] * xs[xs[i+2]]
			i += 4
		} else {
			fmt.Println("Invalid input")
			break
		}
	}
	fmt.Println("Problem 1: ", xs[0])
	return xs[0]
}
