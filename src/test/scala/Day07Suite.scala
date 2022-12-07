package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day07Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
    $ cd /
	$ ls
	dir a
	14848514 b.txt
	8504156 c.dat
	dir d
	$ cd a
	$ ls
	dir e
	29116 f
	2557 g
	62596 h.lst
	$ cd e
	$ ls
	584 i
	$ cd ..
	$ cd ..
	$ cd d
	$ ls
	4060174 j
	8033020 d.log
	5626152 d.ext
	7214296 k
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day07 Part 1") {
		assert(Day07.part1(Day07.parseLines(sampleInput)) == 95437)
	}
	
	test("Day07 Part 2") {
		assert(Day07.part2(Day07.parseLines(sampleInput)) == 24933642)
	}
}