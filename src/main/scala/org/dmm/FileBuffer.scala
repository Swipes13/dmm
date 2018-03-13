package org.dmm

class FileBuffer(fileName: String) {
  private val fb = io.Source.fromFile(fileName)
  private val buffer = try fb.mkString.split(Array('\n', '\t', ' ')) finally fb.close()
  private var pos = -1
  def next(): Int = {
    pos = Math.min(pos + 1, buffer.length)
    buffer(pos).toInt
  }
}
