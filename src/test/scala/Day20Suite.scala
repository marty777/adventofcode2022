package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day21Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	root: pppw + sjmn
	dbpl: 5
	cczh: sllz + lgvd
	zczc: 2
	ptdq: humn - dvpt
	dvpt: 3
	lfqf: 4
	humn: 5
	ljgn: 2
	sjmn: drzm * dbpl
	sllz: 4
	pppw: cczh / lfqf
	lgvd: ljgn * ptdq
	drzm: hmdt - zczc
	hmdt: 32
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day21 Part 1") { 
		assert(Day21.part1(sampleInput.flatMap(Day21.parseMonkey)) == 152)
	}
	
	test("Day21 Part 2") {
		assert(Day21.part2(sampleInput.flatMap(Day21.parseMonkey)) == 301)
	}
}