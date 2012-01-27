package com.github.chuwb.pictbliz

import java.io.File

object Resource {

  val loader = getClass.getClassLoader
  val root = loader.getResource(".")

  def uri(path: String) = loader.getResource(path).toURI
  def file(path: String) = new File( uri(path) )
  def str(path: String) = loader.getResource(path).getPath.toString

  val tempdir = "./temp/"
  
}
