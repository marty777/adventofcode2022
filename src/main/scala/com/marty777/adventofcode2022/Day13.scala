package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day13Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day13Definitions {
	enum Comparison: 
		case LT, GT, EQ
}

object Day13 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath).filter(s => s.length > 0)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(lines: Seq[String]): Int = {
		lines.sliding(2,2).toList.zipWithIndex.foldLeft(0)((acc,entry) => {
			if(comparePackets(entry._1(0), entry._1(1)) == Comparison.LT) acc + (entry._2 + 1)
			else acc
		})
	}
	
	override def part2(lines: Seq[String]): Int = {
		var extendedLines = lines :+ "[[2]]" :+ "[[6]]"
		extendedLines.sortWith(comparePackets(_,_) == Comparison.LT).zipWithIndex.foldLeft(1)((acc, entry) => {
			if(entry._1 == "[[2]]" || entry._1 == "[[6]]") acc * (entry._2 + 1)
			else acc
		})
	}
	
	// is the substring a single value or a list?
	def isValue(str:String):Boolean = (str.length > 0 && str(0) != '[')
	
	def toSubPackets(str:String): Seq[String] = {
		if(isValue(str)) {
			Seq(str)
		}
		else {
			// empty list
			if(str.length < 3) {
				Seq()
			}
			else {
				var elements:Seq[String] = Seq()
				var level = 0
				var index = 1
				var lastIndex = 0
				while(index < str.length - 1) {
					if(str(index) == '[') level += 1
					if(str(index) == ']') level -= 1
					if(str(index) == ',' && level == 0) {
						val substr = str.substring(lastIndex+1, index)
						elements = elements :+ substr
						lastIndex = index
					}
					index += 1
				}
				elements = elements :+ str.substring(lastIndex + 1, index)
				elements
			}
		}
	}

	def comparePackets(left:String, right:String):Comparison = {
		if(isValue(left) && isValue(right)) {
			if(left.toInt < right.toInt) Comparison.LT
			else if(left.toInt > right.toInt) Comparison.GT
			else Comparison.EQ				
		}
		else if(isValue(left) && !isValue(right)) {
			comparePackets(s"[$left]", right)
		}
		else if(!isValue(left) && isValue(right)) {
			comparePackets(left, s"[$right]")
		}
		else {
			var comp = Comparison.EQ
			var index = 0
			val leftList = toSubPackets(left.substring(0, left.length))
			val rightList = toSubPackets(right.substring(0, right.length))
			if(leftList.size > rightList.size) {
				while(index < rightList.size && comp == Comparison.EQ) {
					comp = comparePackets(leftList(index), rightList(index))
					index += 1
				}
				if(comp == Comparison.EQ || comp == Comparison.GT) comp = Comparison.GT
				else comp = Comparison.LT
			}
			else if(leftList.size < rightList.size) {
				while(index < leftList.size && comp == Comparison.EQ) {
					comp = comparePackets(leftList(index), rightList(index))
					index += 1
				}
				if(comp == Comparison.EQ || comp == Comparison.LT) comp = Comparison.LT
				else comp = Comparison.GT
			}
			else {
				while(index < leftList.size && comp == Comparison.EQ) {
					comp = comparePackets(leftList(index), rightList(index))
					index += 1
				}
			}
			comp
		}
	}
}
