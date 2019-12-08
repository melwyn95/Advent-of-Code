package main

import (
	"fmt"
	"strconv"
)

func Solve2(answer int, input string) int {
	noun, verb, found := 0, 0, false
	for ; noun < 100; noun++ {
		for ; verb < 100; verb++ {
			s := string(input[:2]) + strconv.Itoa(noun) + "," + strconv.Itoa(verb) + "," + string(input[6:])
			if Solve1(s) == answer {
				found = true
				break
			}
		}
		if found {
			break
		}
		verb = 0
	}
	fmt.Println("Problem 2: ", noun*100+verb)
	return noun*100 + verb
}
