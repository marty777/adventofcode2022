package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day02Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day02Definitions {
	enum Shape:
		case Rock, Paper, Scissors
	enum Result:
		case Win, Loss, Tie
	case class GuideEntry(opponent: Shape, response:Shape, result:Result)
}
object Day02 extends PuzzleDay[Seq[GuideEntry], Seq[GuideEntry], Int, Int] {
	override def parse1(inputPath: String): Seq[GuideEntry] = readLines(inputPath).flatMap(parseGuideEntry1)
	override def parse2(inputPath: String): Seq[GuideEntry] = readLines(inputPath).flatMap(parseGuideEntry2)
	
	override def part1(entries: Seq[GuideEntry]): Int = {
		entries.map(roundScore).sum
	}
	
	override def part2(entries: Seq[GuideEntry]): Int = {
		entries.map(roundScore).sum
	}
	
	def abcToShape(abc:String):Shape = {
		abc match {
			case "A" => Shape.Rock
			case "B" => Shape.Paper
			case "C" => Shape.Scissors
			case _	=> throw Exception(s"Unknown shape found: $abc")
		}
	}
	
	def xyzToShape(xyz:String):Shape = {
		xyz match {
			case "X" => Shape.Rock
			case "Y" => Shape.Paper
			case "Z" => Shape.Scissors
			case _	=> throw Exception(s"Unknown shape found: $xyz")
		}
	}
	
	def xyzToResult(xyz:String):Result = {
		xyz match {
			case "X" => Result.Loss
			case "Y" => Result.Tie
			case "Z" => Result.Win
			case _	=> throw Exception(s"Unknown shape found: $xyz")
		}
	}
	
	def resultFromShapes(opponent:Shape, response:Shape): Result = {
		opponent match {
			case Shape.Rock 		=> if(response == Shape.Rock) then Result.Tie else if(response == Shape.Paper) then Result.Win else Result.Loss
			case Shape.Paper 		=> if(response == Shape.Rock) then Result.Loss else if(response == Shape.Paper) then Result.Tie else Result.Win
			case Shape.Scissors 	=> if(response == Shape.Rock) then Result.Win else if(response ==Shape.Paper) then Result.Loss else Result.Tie
		}
	}
	
	def shapeFromResult(opponent:Shape, result:Result): Shape = {
		opponent match {
			case Shape.Rock 		=> if(result == Result.Loss) then Shape.Scissors else if(result == Result.Tie) then Shape.Rock else Shape.Paper
			case Shape.Paper 		=> if(result == Result.Loss) then Shape.Rock else if(result == Result.Tie) then Shape.Paper else Shape.Scissors
			case Shape.Scissors 	=> if(result == Result.Loss) then Shape.Paper else if(result == Result.Tie) then Shape.Scissors else Shape.Rock
		}
	}
	
	def parseGuideEntry1(input: String): Option[GuideEntry] = {
		val entry = "(.*) (.*)".r
		input match {
			case entry(opponent, response) => 	{
													val opponentShape = abcToShape(opponent)
													val responseShape = xyzToShape(response)
													Some(GuideEntry(opponentShape,responseShape, resultFromShapes(opponentShape, responseShape)))
												}
			case _				=> None
		}
	}
	
	def parseGuideEntry2(input: String): Option[GuideEntry] = {
		val entry = "(.*) (.*)".r
		input match {
			case entry(opponent, result) => 	{
													val opponentShape = abcToShape(opponent)
													val matchResult = xyzToResult(result)
													Some(GuideEntry(opponentShape,shapeFromResult(opponentShape, matchResult), matchResult))
												}
			case _				=> None
		}
	}
	
	def resultScore(result:Result): Int = {
		result match {
			case Result.Win	=> 6
			case Result.Tie	=> 3
			case Result.Loss	=> 0
		}
	}
	
	def shapeScore(shape:Shape): Int = {
		shape match {
			case Shape.Rock		=> 1
			case Shape.Paper		=> 2
			case Shape.Scissors	=> 3
		}
	}
	
	def roundScore(entry:GuideEntry): Int = {
		shapeScore(entry.response) + resultScore(entry.result)
	}
	
	
}