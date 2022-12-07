package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day07Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day07Definitions {
	
	case class FileNode(symbol:String, size:Int, parentNode:String)
	case class DirNode(symbol:String, parentNode:String, var childFiles:Seq[FileNode])
	
}

object Day07 extends PuzzleDay[Map[String, DirNode], Map[String, DirNode], Int, Int] {
	override def parse1(inputPath: String): Map[String, DirNode] = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): Map[String, DirNode] = parse1(inputPath)
	
	override def part1(nodes: Map[String, DirNode]): Int = {
		nodes.toSeq.filter((key,value) => dirSize(nodes, key) <= 100000 ).map((key, dir) => dirSize(nodes,key)).sum
	}
	override def part2(nodes: Map[String, DirNode]): Int = {
		var minSize = 30000000 - (70000000 - dirSize(nodes, "/"))
		nodes.toSeq.filter((key,value) => dirSize(nodes, key) >= minSize ).map((key,value) => dirSize(nodes, key)).sorted.head
	}
	
	def dirSize(nodes: Map[String, DirNode], symbol:String): Int = {
		var childFileSum = nodes(symbol).childFiles.map(_.size).sum
		var childDirs = nodes.toSeq.filter((k,x) => x.parentNode == symbol).map(_._1)
		var childSum:Int = 0
		for(childDir <- childDirs) {
			childSum += dirSize(nodes, childDir)
		}
		childFileSum + childSum		
	}
	
	def parseLines(lines:Seq[String]): Map[String, DirNode] = {
		var index = 0
		var fileNodes:Map[String, DirNode] = Map()
		val cd = raw"""\$$ cd (.*)""".r
		// add root node
		fileNodes = fileNodes.updated("/", DirNode("/", "", Seq()))
		var parentSymbol = ""
		while(index < lines.size) {
			lines(index) match {
				case cd(symbol) => {
					if(symbol == "..") {
						parentSymbol = fileNodes(parentSymbol).parentNode
						index += 1
					}
					else {
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
								fileNodes = fileNodes.updated(newKey, DirNode(split(1), parentSymbol, Seq()))
							}
							else {
								fileNodes(parentSymbol).childFiles =  fileNodes(parentSymbol).childFiles.appended(FileNode(split(1), split(0).toInt, parentSymbol))
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