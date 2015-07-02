package se.fnord.conway

import akka.actor.Actor

trait EventStreamPublisher {
  this: Actor =>
  def publish(event : AnyRef) = context.system.eventStream.publish(event)
}

trait EventStreamSubscriber {
  this: Actor =>
  def subscribe(channel : Class[_]) = context.system.eventStream.subscribe(context.self, channel)
  def unsubscribe(channel : Class[_]) = context.system.eventStream.unsubscribe(context.self, channel)
}
