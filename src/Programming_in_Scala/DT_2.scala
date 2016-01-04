package Programming_in_Scala
import java.io.File
import collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Takechiyo on 2016/1/4.
  * WordCount Program
  */
object DT_2 {
  /**
    * 遍历目录 得到目录下所有文件的绝对路径
    * @param file  The file or directory needed to transverse.
    * @param fileList  All of the file's name in the directory.
    */
  def walk(file: File, fileList: ArrayBuffer[String]): Unit ={
    if (file.isFile()){
      println(file.getAbsolutePath)
      fileList += file.getAbsolutePath
    }
    else
      file.listFiles().foreach(walk(_, fileList))
  }


  def reduceByKey[K](collection: Traversable[Tuple2[K, Int]]) = {
    collection
      .groupBy(_._1)
      .map { case (group: K, traversable) => traversable.reduce{(a,b) => (a._1, a._2 + b._2)} }
  }

  def main(args: Array[String]) {
    val directory = "D:\\temp"
    val fileList = ArrayBuffer[String]()
    walk(new File(directory), fileList)
    //读取文件中的内容，并分词，生成(String, 1)
    val result = fileList
                    .flatMap(Source.fromFile(_).getLines())
                    .flatMap(_.split(" "))
                    .map(item => (item, 1))
    //统计目录下所有文件中每个单词出现的总次数
    reduceByKey(result).foreach(println)
    //统计目录下所有文件中单词的总个数
    val count = result.map(_ => 1).reduce(_ + _)
    println(count)
  }
}
