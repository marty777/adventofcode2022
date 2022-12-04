package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day04Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day04Definitions {
	case class RangePair(start1:Int, end1:Int, start2:Int, end2:Int)
}

object Day04 extends PuzzleDay[Seq[RangePair], Seq[RangePair], Int, Int] {
	override def parse1(inputPath: String): Seq[RangePair] = readLines(inputPath).flatMap(parseRangePair)
	override def parse2(inputPath: String): Seq[RangePair] = parse1(inputPath)
	
	override def part1(rangePairs: Seq[RangePair]): Int = rangePairs.map(containedRangePair).count(_ == true)
	override def part2(rangePairs: Seq[RangePair]): Int = rangePairs.map(overlappedRangePair).count(_ == true)
	
	// succinct and hopefully clear, but determining this algebraically is so 
	// much more efficient it's hard to justify
	def containedRangePair(rangePair:RangePair): Boolean = {
		val set1 = (rangePair.start1 to rangePair.end1).toSet
		val set2 = (rangePair.start2 to rangePair.end2).toSet
		set1.subsetOf(set2) || set2.subsetOf(set1)
	}
	
	def overlappedRangePair(rangePair:RangePair): Boolean = {
		!(rangePair.start1 to rangePair.end1).intersect(rangePair.start2 to rangePair.end2).isEmpty
	}
	
	def parseRangePair(input: String): Option[RangePair] = {
		val rangePair = "(.*)-(.*),(.*)-(.*)".r
		input match {
			case rangePair(start1, end1, start2, end2)	=> 	Some(RangePair(start1.toInt,end1.toInt,start2.toInt,end2.toInt))
			case _	=>	None
		}
	}	
}