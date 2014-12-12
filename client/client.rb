# Copyright © 2014 Garrison Jensen
# License
# This code and text are dedicated to the public domain.
# You can copy, modify, distribute and perform the work,
# even for commercial purposes, all without asking permission.

require "socket"

class Client
    def initialize(server, port)
        @server    = server
        @port      = port
        @socket    = TCPSocket.open(@server, @port)
        @listening = false
        @listener  = nil
    end
    def transmit(message)
      begin
        @socket.puts(message)
        "" # No Error
      rescue Errno::EPIPE
        "ERROR: Server is unreachable. Please restart application."
      end
    end
    def add_listener(listener)
       listen unless @listening == true
       @listener = listener 
      
    end
    def listen
        @response = Thread.new do
            loop {
                msg = @socket.gets.chomp
                @listener.new_message(msg)
            }
        end
        @listening = true
    end
end

