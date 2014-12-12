# Copyright © 2014 Garrison Jensen
# License
# This code and text are dedicated to the public domain.
# You can copy, modify, distribute and perform the work,
# even for commercial purposes, all without asking permission.

class Message
    attr_reader :from, :to, :msg, :text, :command
    def initialize(message)
        if message.include?("PRIVMSG")
            @from    = (message[1..-1].split)[0]
            @to      = (message.split)[2].delete(":")
            @msg     = message[1..-1].split[3..-1].join(" ")
            @command = "PRIVMSG"
            @text    = message
        elsif message.start_with?(":Successfully joined ")
            @from    = "Server"
            @to      = nil
            @msg     = message.split[2..-1].join(" ").delete(":")
            @command = "Successfull Join"
            @text    = message
        elsif message.start_with?(":Successfully left ")
            @from    = "Server"
            @to      = nil
            @msg     = message.split[2].delete(":")
            @command = "Successfull Part"
            @text    = message
        else
            @from    = nil
            @to      = nil
            @msg     = message.gsub(/^:.*:/, "").lstrip
            @command = nil
            @text    = message
        end
    end
end
