package main

import "fmt"

func pathToCOM(m *map[string]string, k string, path *[]string) {
	if (*m)[k] == "COM" {
		*path = append(*path, "COM")
		return
	}
	*path = append(*path, (*m)[k])
	pathToCOM(m, (*m)[k], path)
}

func Solve2(s string) int {
	m := parseInput(s)
	p1 := []string{}
	pathToCOM(&m, "YOU", &p1)
	p2 := []string{}
	pathToCOM(&m, "SAN", &p2)

	i, j := len(p1)-1, len(p2)-1
	for {
		if p1[i] != p2[j] {
			break
		}
		i--
		j--
	}

	fmt.Println("Problem 2:", i+j+2)

	return i + j
}
