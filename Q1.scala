package LabSheet_06
import scala.io.StdIn

object Q1 {
   def caesarEncrypt(plainText: String, shift: Int): String = {
      val encryptedText = plainText.map {char =>
        if(char.isLetter){
          val start = if(char.isUpper) 'A' else 'a'
          val shiftedChar = (start + (char - start +shift) % 26).toChar
          shiftedChar
        }else{
          char
        }
      }
     encryptedText
   }

  def caesarDecrypt(plainText: String, shift: Int): String = {
    val decryptedText = plainText.map { char =>
      if (char.isLetter) {
        val start = if (char.isUpper) 'A' else 'a'
        val shiftedChar = (start + (char - start - shift) % 26).toChar
        shiftedChar
      } else {
        char
      }
    }
    decryptedText
  }

   def cipher(data: String, shift: Int, cipherFunc: (String, Int) => String): String = {
      cipherFunc(data, shift)
   }

   def main(args: Array[String]):Unit={
      println("Enter the text : ")
      val textToEncrypt = StdIn.readLine()

      println("Enter the shift amount : ")
      val shiftAmount = StdIn.readInt()

      val encryptedText = cipher(textToEncrypt, shiftAmount, caesarEncrypt)
      println("Encrypted text : " +encryptedText)

      val decryptedText = cipher(encryptedText, shiftAmount, caesarDecrypt)
      println("Decrypted text : "  +decryptedText)
  }
}
