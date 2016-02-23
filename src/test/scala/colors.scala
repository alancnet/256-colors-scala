import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import org.scalatest._
import io.AnsiColor256._

class ColorTest extends FlatSpec {
  def getPixels(img: BufferedImage) = {
    val (width, height) = (img.getWidth, img.getHeight)
    def readCol(r:Int, c: Int): Stream[Int] =
      if (c >= width) Stream.empty
      else img.getRGB(c, r) #:: readCol(r, c + 1)
    def readRow(r:Int):Stream[Iterable[Int]] =
      if (r >= height) Stream.empty
      else readCol(r, 0).toIterable #:: readRow(r + 1)
    readRow(0).toIterable
  }

  println(s"${rgb(1.0, 1.0, 1.0)}findme${RESET}")
  val photo1 = ImageIO.read(new File("/home/alco/Downloads/face.png"))


  def pairs[T](it: Iterable[T]):Iterable[(T, Option[T])] = {
    def loop(it: Iterable[T]): Stream[(T, Option[T])] = {
      it.headOption match {
        case Some(head) => it.tail.headOption match {
          case Some(head2) => (head, Some(head2)) #:: loop(it.tail.tail)
          case None => (head, None) #:: Stream.empty
        }
        case None => Stream.empty
      }
    }
    loop(it).toIterable
  }

  def getAnsiImage(img:BufferedImage) = pairs(getPixels(img)).map {
    case (row1, Some(row2)) => row1.zip(row2).map{
      case (c1, c2) => s"${BOLD}${rgb_b(c1)}${rgb(c2)}▅"
    }.mkString("")
  }.mkString(s"${RESET}\r\n") + RESET

//  ((row1, row2) => {
//    row1.map(c => s"${rgb_b(c)} ").mkString("")
//  }).mkString(s"${RESET}\r\n") + RESET

  val alan = getAnsiImage(photo1)

  println("echo -e \"" + alan.replace("\u001b", "\\E").replace("\r", "\\r").replace("\n", "\\n").replace("▅", "\\u2585") + "\"")
};