package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day25Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	1=-0-2
	12111
	2=0=
	21
	2=01
	111
	20012
	112
	1=-1=
	1-12
	12
	1=
	122
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day25 Part 1") { 
		assert(Day25.part1(sampleInput) == "2=-1=0")
	}
}