package main

import (
	"fmt"
	"strings"
)

func parseInput(s string) map[string]string {
	xs := strings.Split(s, "\n")
	ys := map[string]string{}
	for i := range xs {
		split := strings.Split(xs[i], ")")
		ys[split[1]] = split[0]
	}
	return ys
}

func search(m *map[string]string, k string, count int) int {
	if (*m)[k] == "COM" {
		return count
	}
	return search(m, (*m)[k], count+1)
}

func Solve1(s string) int {
	m := parseInput(s)
	count := 0
	for k := range m {
		count += search(&m, k, 1)
	}
	fmt.Println("Problem 1:", count)
	return count
}
