package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day11Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines
import scala.math.BigInt

object Day11Definitions {
	case class Monkey(id: Int, op:String, opVal:Int, opValIsOld:Boolean, testDivisor:Int, trueThrow:Int, falseThrow:Int, var inspections:Int = 0)
	case class Item(id: Int, var monkeyId:Int, var value:BigInt, var order:Int)
	case class State(monkeys:Seq[Monkey], items:Seq[Item], gcd:Int, var round:Int = 0)
}

object Day11 extends PuzzleDay[State, State, BigInt, BigInt] {
	override def parse1(inputPath: String): State = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): State = parse1(inputPath)
	
	override def part1(state: State): BigInt = {
		for(round <- 1 to 20) {
			runMonkeyRound(state, part2 = false)
		}
		state.monkeys.map(_.inspections).sorted.reverse.take(2).foldLeft(1:BigInt)(_ * _)
	}
	
	override def part2(state: State): BigInt = {
		for(round <- 1 to 10000) {
			runMonkeyRound(state, part2 = true)
		}
		state.monkeys.map(_.inspections).sorted.reverse.take(2).foldLeft(1:BigInt)(_ * _)
	}
	
	def runMonkeyRound(state:State, part2:Boolean = false): Unit = {
		state.round = state.round + 1
		for(i <- 0 until state.monkeys.size) {
			var items = state.items.filter(x => x.monkeyId == i).sortBy(_.order)
			for(j <- 0 until items.size) {
				state.monkeys(i).inspections += 1
				var itemValue = items(j).value
				if(state.monkeys(i).opValIsOld) {
					itemValue = itemValue * itemValue
				}
				else {
					if(state.monkeys(i).op == "+") {
						itemValue = itemValue + state.monkeys(i).opVal
					}
					else {
						itemValue = itemValue * state.monkeys(i).opVal
					}
				}
				if(part2) {
					itemValue = itemValue % state.gcd
					
				}
				else {
					itemValue = itemValue / 3
				}
				state.items(items(j).id).value = itemValue
				if(itemValue % state.monkeys(i).testDivisor == 0) {
					var order = 0
					val orders = state.items.filter(_.monkeyId == state.monkeys(i).trueThrow).map(_.order)
					if(orders.size == 0) {
						order = 0
					}
					else {
						order = orders.max + 1
					}
					state.items(items(j).id).monkeyId = state.monkeys(i).trueThrow
					state.items(items(j).id).order = order
				}
				else {
					var order = 0
					val orders = state.items.filter(_.monkeyId == state.monkeys(i).trueThrow).map(_.order)
					if(orders.size == 0) {
						order = 0
					}
					else {
						order = orders.max + 1
					}
					state.items(items(j).id).monkeyId = state.monkeys(i).falseThrow
					state.items(items(j).id).order = order
				}
			}
		}
	}
	
	def parseLines(lines:Seq[String]): State = {
		var index = 0
		val startingItems = "  Starting items: (.*)".r
		val operation = "  Operation: new = old (.*) (.*)".r
		val test = "  Test: divisible by (.*)".r
		val trueTest = "    If true: throw to monkey (.*)".r
		val falseTest = "    If false: throw to monkey (.*)".r
		var monkeys:Seq[Monkey] = Seq()
		var items: Seq[Item] = Seq()
		var monkeyId = 0
		var gcd = 1
		while(index < lines.size) {
			index += 1
			var theOp:String = ""
			var theOpVal:Int = 0
			var theOpValIsOld = false
			var theTestDivisor:Int = 0
			var theTrueThrow:Int = 0
			var theFalseThrow:Int = 0
			var doneMonkey = false
			while(!doneMonkey) {
				lines(index) match {
					case startingItems(itemList) => {
						val theItems = itemList.split(", ").map(BigInt(_))
						for(i <- 0 until theItems.size) {
							items = items :+ Item(items.size, monkeyId, theItems(i), i)
						}
						index += 1
					}
					case operation(op, opval) => {
						theOp = op
						if(opval == "old") {
							theOpValIsOld = true
							theOpVal = 0
						}
						else {
							theOpVal = opval.toInt
							theOpValIsOld = false
						}
						index += 1
					}
					case test(divisor) => {
						theTestDivisor = divisor.toInt
						index += 1
					}
					case trueTest(trueThrow) => {
						theTrueThrow = trueThrow.toInt
						index += 1
					}
					case falseTest(falseThrow) => {
						theFalseThrow = falseThrow.toInt
						monkeys = monkeys :+ Monkey(monkeyId, theOp, theOpVal, theOpValIsOld, theTestDivisor, theTrueThrow, theFalseThrow)
						gcd *= theTestDivisor
						index += 2
						doneMonkey = true
						monkeyId += 1
					}
				}
			}
		}
		// gcd should be checked, but by inspection all input divisors are prime
		State(monkeys, items, gcd)
	}
}
