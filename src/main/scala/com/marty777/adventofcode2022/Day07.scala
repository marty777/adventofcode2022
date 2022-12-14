package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day07Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day07Definitions {
	case class DirNode(symbol:String, parentNode:String, var childFileSum:Int = 0, var totalSum:Int = -1)
}

object Day07 extends PuzzleDay[Map[String, DirNode], Map[String, DirNode], Int, Int] {
	override def parse1(inputPath: String): Map[String, DirNode] = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): Map[String, DirNode] = parse1(inputPath)
	
	override def part1(nodes: Map[String, DirNode]): Int = {
		nodes.keys.toSeq.map(key => dirSize(nodes, key)).filter(size => size <= 100000).sum
	}
	override def part2(nodes: Map[String, DirNode]): Int = {
		var minSize = 30000000 - (70000000 - dirSize(nodes, "/"))
		nodes.keys.map(dirSize(nodes, _)).filter(size => size >= minSize).toSeq.sorted.head
	}
	
	def dirSize(nodes: Map[String, DirNode], symbol:String): Int = {
		if(nodes(symbol).totalSum >= 0) {
			nodes(symbol).totalSum
		}
		else {
			var childDirs = nodes.toSeq.filter((k,x) => x.parentNode == symbol).map(_._1)
			val childDirSum = childDirs.foldLeft(0){_ + dirSize(nodes, _)}
			val totalSize = nodes(symbol).childFileSum + childDirSum	
			nodes(symbol).totalSum = totalSize
			totalSize
		}
	}
	
	def parseLines(lines:Seq[String]): Map[String, DirNode] = {
		var index = 0
		var fileNodes:Map[String, DirNode] = Map()
		val cd = raw"""\$$ cd (.*)""".r
		// add root node
		fileNodes = fileNodes.updated("/", DirNode("/", ""))
		var parentSymbol = ""
		while(index < lines.size) {
			lines(index) match {
				case cd(symbol) => {
					if(symbol == "..") {
						parentSymbol = fileNodes(parentSymbol).parentNode
						index += 1
					}
					else {
						// this is a mess but at least it works
						index = index + 2
						if(symbol == "/") {
							parentSymbol = "/"
						}
						else {
							if(parentSymbol == "/") {
								parentSymbol = "/" + symbol
							}
							else {
								parentSymbol = parentSymbol + "/" + symbol
							}
						}
						while(index < lines.size && cd.findAllIn(lines(index)).size == 0) {
							val split = lines(index).split(" ")
							if(split(0) == "dir") {
								var newKey = parentSymbol + "/" + split(1)
								if(parentSymbol == "/") {
									newKey = "/" + split(1)
								}
								fileNodes = fileNodes.updated(newKey, DirNode(split(1), parentSymbol))
							}
							else {
								fileNodes(parentSymbol).childFileSum +=  split(0).toInt
							}
							index += 1
						}
					}
				}
			}
		}
		fileNodes
	}
}
