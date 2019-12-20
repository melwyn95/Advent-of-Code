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

func IntCode(input string, params []int) int {
	xs := parseInput(input)
	L := len(xs)
	io := 0
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
			xs[xs[i+1]] = params[io]
			io++
			i += 2
		} else if opcode == 4 {
			// can be in immediate mode...
			if d[1] == 1 {
				return xs[i+1]
			} else {
				return xs[xs[i+1]]
			}
			i += 2
		} else if opcode == 5 {
			if d[1] == 0 {
				if xs[xs[i+1]] != 0 {
					if d[2] == 0 {
						i = xs[xs[i+2]]
					} else {
						i = xs[i+2]
					}
				} else {
					i += 3
				}
			} else {
				if xs[i+1] != 0 {
					if d[2] == 0 {
						i = xs[xs[i+2]]
					} else {
						i = xs[i+2]
					}
				} else {
					i += 3
				}
			}
		} else if opcode == 6 {
			if d[1] == 0 {
				if xs[xs[i+1]] == 0 {
					if d[2] == 0 {
						i = xs[xs[i+2]]
					} else {
						i = xs[i+2]
					}
				} else {
					i += 3
				}
			} else {
				if xs[i+1] == 0 {
					if d[2] == 0 {
						i = xs[xs[i+2]]
					} else {
						i = xs[i+2]
					}
				} else {
					i += 3
				}
			}
		} else if opcode == 7 {
			if d[1] == 0 && d[2] == 0 {
				if xs[xs[i+1]] < xs[xs[i+2]] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 0 && d[2] == 1 {
				if xs[xs[i+1]] < xs[i+2] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 1 && d[2] == 0 {
				if xs[i+1] < xs[xs[i+2]] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 1 && d[2] == 1 {
				if xs[i+1] < xs[i+2] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			}
			i += 4
		} else if opcode == 8 {
			if d[1] == 0 && d[2] == 0 {
				if xs[xs[i+1]] == xs[xs[i+2]] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 0 && d[2] == 1 {
				if xs[xs[i+1]] == xs[i+2] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 1 && d[2] == 0 {
				if xs[i+1] == xs[xs[i+2]] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			} else if d[1] == 1 && d[2] == 1 {
				if xs[i+1] == xs[i+2] {
					xs[xs[i+3]] = 1
				} else {
					xs[xs[i+3]] = 0
				}
			}
			i += 4
		} else {
			fmt.Println("Invalid input")
			break
		}
	}
	return -1
}
