package com.marty777.util

import scala.io.Source
import scala.util.{Failure, Success, Try}

object FileLines {

	/**
	* Read a text file from the given path and yield a sequence of strings, one per line
	*/
	def readLines(inputPath: String): Seq[String] = {
		Try(Source.fromFile(inputPath).getLines()) match {
			case Success(lines) => lines.toSeq
			case Failure(e) => { 
				println(s"An error has occured, cause: $e") 
				null
			}
		}
	}
}