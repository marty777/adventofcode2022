package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day01 extends PuzzleDay[Seq[Int], Seq[Int], Int, Int] {
	override def parse1(inputPath: String): Seq[Int] = parseElves(readLines(inputPath))
	override def parse2(inputPath: String): Seq[Int] = parse1(inputPath)
	
	override def part1(elves: Seq[Int]): Int = {
		elves.max
	}
	
	override def part2(elves: Seq[Int]): Int = {
		elves.sorted.takeRight(3).sum
	}
	
	def parseElves(lines: Seq[String]): Seq[Int] = {
		var elfSums:collection.mutable.Seq[Int] = collection.mutable.Seq(0)
		for (line <- lines) {
			if(line.length == 0) {
				elfSums = elfSums :+ 0
			}
			else {
				elfSums.update(elfSums.size - 1, elfSums(elfSums.size - 1) + line.toInt)
			}
		}
		elfSums.toSeq
	}
}