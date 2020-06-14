package aia.stream

import spray.json._

trait MetricMarshalling extends EventMarshalling with DefaultJsonProtocol {
  implicit val metric = jsonFormat5(Metric)
}
