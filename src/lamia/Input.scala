package lamia

import java.io.Reader

class Input(in: Reader) {
  
  private var _ch = '\0'
  def ch = _ch
  
  def next() = {
    val i = in.read()
    if (i == -1) {
      _ch = '\0'
      false
    }
    else {
      _ch = i.toChar
      true
    }
  }
  
  next()
  
}