package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day22Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5""".split(fileNewline).toSeq

	test("Day22 Part 1") { 
		assert(Day22.part1(sampleInput) == 6032)
	}
	
	test("Day22 Part 2") {
		assert(Day22.part2(sampleInput) == 5031)
	}
}