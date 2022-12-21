package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day21Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day21Definitions {
	enum Op:
		case Add,Sub,Mult,Div,Nil
	case class Monkey(name:String, value:Long, left:String, right:String, op:Op)
}

object Day21 extends PuzzleDay[Seq[Monkey], Seq[Monkey], Long, Long] {
	override def parse1(inputPath: String): Seq[Monkey] = readLines(inputPath).flatMap(parseMonkey)
	override def parse2(inputPath: String): Seq[Monkey] = parse1(inputPath)

	override def part1(monkeys: Seq[Monkey]): Long =  {
		rootMonkey(monkeys, part2=false, 0)._1
	}
	
	override def part2(monkeys: Seq[Monkey]): Long =  {
		bsearch(monkeys)
	}
	
	// if not part2, returns (rootvalue, 0)
	// if part 2, returns (root.leftvalue, root.rightvalue)
	def rootMonkey(monkeys:Seq[Monkey], part2:Boolean, humn:Long): (Long, Long) = {
		val monkeyMap = monkeys.map(m => m.name -> m).toMap
		val cache = scala.collection.mutable.Map.empty[String, Long]
		val unexplored = scala.collection.mutable.Set.empty[Monkey]
		for(m <- monkeys) {
			unexplored += m
		}
		while(unexplored.size > 0) {
			for(unexploredMonkey <- unexplored) {
				if(unexploredMonkey.op == Op.Nil) {
					if(part2 && unexploredMonkey.name == "humn") {
						cache(unexploredMonkey.name) = humn
					}
					else {
						cache(unexploredMonkey.name) = unexploredMonkey.value
					}
					unexplored -= unexploredMonkey
				}
				else if(cache.contains(unexploredMonkey.left) && cache.contains(unexploredMonkey.right)) {
					unexploredMonkey.op match {
						case Op.Add => cache(unexploredMonkey.name) = cache(unexploredMonkey.left) + cache(unexploredMonkey.right)
						case Op.Sub => cache(unexploredMonkey.name) = cache(unexploredMonkey.left) - cache(unexploredMonkey.right)
						case Op.Mult => cache(unexploredMonkey.name) = cache(unexploredMonkey.left) * cache(unexploredMonkey.right)
						case Op.Div => cache(unexploredMonkey.name) = cache(unexploredMonkey.left) / cache(unexploredMonkey.right)
						case _ => throw Exception(s"unexpected nil op in unexploredMonkey $unexploredMonkey")
					}
					unexplored -= unexploredMonkey
				}
			}
		}
		if(part2) {
			val rootMonkey = monkeys.filter(m => m.name == "root").head
			(cache(rootMonkey.left), cache(rootMonkey.right))
		}
		else {
			(cache("root"), 0)
		}
	}
	
	// I'm not up for building a solver now, so a binary search to determine a zero
	// crossing of (root.left - root.right). The function isn't continuous and I
	// know it has multiple zero points (not multiple zero-crossings per se; there
	// will be a single uninterrupted range of humn values that result in equality) 
	// but it shouldn't be non-convex, making this search suitable. 
	// Because multiple humn values result in equality, but I'm not completely
	// sure all would be accepted as answers, return the minimum because that 
	// matches the example.
	def bsearch(monkeys:Seq[Monkey]):Long = {
		// initial bounds. I couldn't come up with a great way to estimate this
		// based on the input, but am making the assumption that the puzzle
		// solution will be a long value so Long.MinValue,Long.MaxValue are
		// reasonable. Because of issues with performing arithmetic at the
		// extremes and because I don't want to rewrite rootMonkey to use a
		// larger integer type, reduce the bounds by a few orders of magnitude.
		var hi = Long.MaxValue >> 16
		var lo = -hi
		var mid:Long = 0
		var done = false
		while(lo < hi && !done) {
			mid = lo + (hi - lo)/2
			val f_lo = diff(rootMonkey(monkeys, part2 = true, lo)) 
			val f_hi = diff(rootMonkey(monkeys, part2 = true, hi)) 
			val f_mid = diff(rootMonkey(monkeys, part2 = true, mid)) 
			if(f_lo < f_hi) {
				if(f_mid < 0) {
					lo = mid + 1
				}
				else if(f_mid > 0) {
					hi = mid - 1
				}
				else {
					done = true
				}
			}
			else if(f_lo > f_hi) {
				if(f_mid < 0) {
					hi = mid - 1
				}
				else if(f_mid > 0) {
					lo = mid + 1
				}
				else {
					done = true
				}
			}
			else {
				throw Exception(s"Stuck at lo $lo -> $f_lo hi $hi -> $f_hi")
			}
		}
		// if more than one solution exists, return the lowest possible
		var min_result = mid
		while(diff(rootMonkey(monkeys, part2 = true, min_result - 1)) == 0 ) {
			min_result -= 1
		}
		min_result
	}
	
	def diff(v:(Long, Long)):Long = (v._1 - v._2)
	
	def parseMonkey(line:String):Option[Monkey] = {
		val valLine = "(.*): ([0-9]+)".r
		val opLine = "(.*): (.*) (.*) (.*)".r
		line match {
			case valLine(name, valStr) => Some(Monkey(name, valStr.toLong, "", "", Op.Nil))
			case opLine(name, left, op, right) => Some(Monkey(name, 0, left, right, op match {
				case "+" => Op.Add
				case "-" => Op.Sub
				case "*" => Op.Mult
				case "/" => Op.Div
				case _ => throw Exception(s"Unknown operation $op found in line $line")
			}))
			case _ => None
		}
	}
	
}

