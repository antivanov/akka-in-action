package aia.stream

import akka.http.scaladsl.model._

import akka.stream.scaladsl.Source
import akka.util.ByteString

import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.marshalling.ToEntityMarshaller

object LogEntityMarshaller extends EventMarshalling {
  
  type EntityMarshaller = ToEntityMarshaller[Source[ByteString, _]]
  def create(maxJsonObject: Int): EntityMarshaller = {
    val js = ContentTypes.`application/json`
    val txt = ContentTypes.`text/plain(UTF-8)`

    val jsMarshaller: Marshaller[Source[ByteString, _], HttpEntity.Chunked] = Marshaller.withFixedContentType(js) { src: Source[ByteString, _] =>
      HttpEntity(js, src)
    }

    val txtMarshaller: Marshaller[Source[ByteString, _], HttpEntity.Chunked] = Marshaller.withFixedContentType(txt) { src: Source[ByteString, _] =>
      HttpEntity(txt, src.via(LogJson.jsonToLogFlow(maxJsonObject)))
    }

    Marshaller.oneOf(jsMarshaller, txtMarshaller)
  }
}

