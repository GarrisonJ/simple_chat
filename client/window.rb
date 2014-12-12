# Copyright © 2014 Garrison Jensen
# License
# This code and text are dedicated to the public domain.
# You can copy, modify, distribute and perform the work,
# even for commercial purposes, all without asking permission.

require "curses"
require "set"
require "./message"

class Window
    include Curses
    def initialize(client)
        @currentRoom = ""       # The room user's messages will be sent
        @joinedRooms = Set.new  # The rooms we are receiving messages from
        @client      = client   # The client receives and sends messages to server
        @messages    = []       # List of messages received
    end

    # Initialize window
    def start
        # Initialize a standard screen
        init_screen 
         # Initializes the color attributes
        start_color
        # Foreground to black and background to white
        init_pair(COLOR_WHITE, COLOR_BLACK, COLOR_WHITE) 
        # Use terminal’s default colors.
        use_default_colors 
        # Redraw window
        redraw
        # Listen for messages
        @client.add_listener(self)
        # Listen for user input
        while capture_input 
        end
    end

    # The client will send messages to window through this method
    def new_message(message)
        if message == "PING"
            send("PONG")
        else
            @messages << Message.new(message)
            redraw
        end
    end
    private

    # Send messages to server
    def send(message)
        if message.length > 0
            # The client will return an empty string
            # on success, or it will return an error
            error = @client.transmit(message)
            if error.length > 0
                new_message(error)
            end
        end
    end
    
    # Capture user input
    def capture_input
        content = getstr

        if content =~ /\\QUIT|\\quit/
            return false
        elsif content.start_with?("\\")
            msg = content.delete("\\")
        else 
            msg = "PRIVMSG #{@currentRoom} #{content}"
        end

        send(msg)

        redraw
        return true
    end
    
    # Update/refresh window
    def redraw
        draw_messages
        draw_text_field
        cursor_to_input_line
        refresh
    end

    # Draw status bar that's above input line
    def draw_text_field
        setpos(divider_line, 0)
        attron(color_pair(COLOR_WHITE) | A_NORMAL) {
            addstr("Current Room:#{@currentRoom} #{(@joinedRooms.delete(@currentRoom)).to_a}" + " " * cols)
        }
        cursor_to_input_line
        clrtoeol
    end

    # Messages will be drawn to the hight of the screen minus the hight of the status bar and input line
    def draw_messages
        @messages.last(window_line_size).inject(0) do |line_number, message|
            setpos(line_number, 0)
            clrtoeol

            if message.command == "PRIVMSG"
                sendMsg = "#{message.to}: <#{message.from}> #{message.msg}"
            elsif message.command == "Successfull Join"
                @currentRoom = message.msg.split[0]
                @joinedRooms.merge(message.msg.split)
                sendMsg = message.text
            elsif message.command == "Successfull Part"
                @joinedRooms.delete(message.msg)
                if @currentRoom == message.msg then @currentRoom = @joinedRooms.to_a[0] end
                sendMsg = message.text
            else
                sendMsg = message.msg
            end

            if sendMsg != ""
                addstr("> #{sendMsg}")
                line_number = line_number + 1
            end

            line_number
        end
    end

    # Position of input line
    def input_line
        lines - 1
    end
    
    # Position of divider/status bar
    def divider_line
        lines - 2
    end
  
    # End of messages window
    def window_line_size
        lines - 2
    end

    # Send cursor to input line
    def cursor_to_input_line
        setpos(input_line, 0)
    end
end
