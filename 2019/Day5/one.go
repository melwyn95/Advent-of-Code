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

func digits(n int) []int {
	var d []int
	d = append(d, n%100)
	n /= 100
	for n > 0 {
		d = append(d, n%10)
		n /= 10
	}
	for len(d) < 4 {
		d = append(d, 0)
	}
	return d
}

func Solve1(input string) {
	xs := parseInput(input)
	L := len(xs)
	for i := 0; i < L; {
		d := digits(xs[i])
		opcode := d[0]
		if opcode == 99 {
			i++
			break
		} else if opcode == 1 {
			var p1, p2 int
			if d[1] == 1 {
				p1 = xs[i+1]
			} else {
				p1 = xs[xs[i+1]]
			}
			if d[2] == 1 {
				p2 = xs[i+2]
			} else {
				p2 = xs[xs[i+2]]
			}
			xs[xs[i+3]] = p1 + p2
			i += 4
		} else if opcode == 2 {
			var p1, p2 int
			if d[1] == 1 {
				p1 = xs[i+1]
			} else {
				p1 = xs[xs[i+1]]
			}
			if d[2] == 1 {
				p2 = xs[i+2]
			} else {
				p2 = xs[xs[i+2]]
			}
			xs[xs[i+3]] = p1 * p2
			i += 4
		} else if opcode == 3 {
			// take input ...
			var input int
			fmt.Scanf("%d", &input)
			xs[xs[i+1]] = input
			i += 2
		} else if opcode == 4 {
			// can be in immediate mode...
			if d[1] == 1 {
				fmt.Println(xs[i+1])
			} else {
				fmt.Println(xs[xs[i+1]])
			}
			i += 2
		} else {
			fmt.Println("Invalid input")
			break
		}
	}
}
