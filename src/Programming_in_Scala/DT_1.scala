package Programming_in_Scala

/**
  * Created by Takechiyo on 2016/1/1.
  * DT 大数据梦工厂 第一个作业
  * Remove the negative numbers in an array after the first negative number.
  */
object DT_1 {
  def main(args: Array[String]) {
    var flag : Boolean = true
    def isNegative(n : Int) : Boolean = {
      if(flag)
        if(n < 0){
          flag = false
          return true
        }
      n >= 0 || flag
    }
    val a = Array(-1, -1, 2, 3, -4)
    val b = a.filter(isNegative(_))
    b.foreach(x => println(x))
  }
}
