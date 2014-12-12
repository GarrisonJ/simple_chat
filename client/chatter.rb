# Copyright © 2014 Garrison Jensen
# License
# This code and text are dedicated to the public domain.
# You can copy, modify, distribute and perform the work,
# even for commercial purposes, all without asking permission.

require "./client"
require "./window"

class Chatter
  def self.start
    client = nil

    # Try to connect to server.
    # Keep asking user for server name
    # until a good server is found.
    while client == nil
        print "Please enter server: "
        server = gets.chomp
        begin
          client = Client.new(server, 5002)
        rescue 
          client = nil
        end
    end

    window = Window.new(client)
    window.start
  end
end


