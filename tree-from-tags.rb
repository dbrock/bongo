#!/usr/bin/env ruby
## tree-from-tags.rb --- create file hierarchies from media tags
# Copyright (C) 2006, 2007  Daniel Brockman

# Author: Daniel Brockman <daniel@brockman.se>
# Created: April 24, 2006

# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with GNU Emacs; if not, write to the Free
# Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.
#
# To run this program, you need Ruby-taglib, available at
# <https://robinst.github.io/taglib-ruby/>.

require "fileutils"
require "find"
require "taglib"

if ARGV.empty? or ["-?", "-h", "-help", "--help"].include? ARGV.first
  puts "Usage: #$0 [--hardlinks] DIRECTORIES..."
  puts "
This program recursively scans DIRECTORIES for media files in formats
that support embedded file tags, such as Ogg and MP3.

For each file with sufficient embedded information, it creates a symlink
in the current directory, or in a subdirectory of the current directory,
pointing to the original file.

If given the `--hardlinks' option, it creates hardlinks instead.

The symlinks or hardlinks created by this program follow a certain
naming scheme that is understood by Bongo, the Emacs media player."
  exit
end

if ["--hard", "--hardlink", "--hardlinks"].include? ARGV.first
  ARGV.shift
  $hardlinks = true
else
  $hardlinks = false
end

class NotEnoughData < RuntimeError ; end

class String
  def blank? ; !self[/\S/] end
  def trim ; sub(/^\s+|\s+$/, "") end
end

def escape_component (component)
  case component
  when "." then "Dot"
  when ".." then "Double Dot"
  else component.gsub(" - ", " -- ").gsub("/", "\\")
  end
end

def join_components (*components)
  components.compact * " - "
end

def parse_data (data)
  an = data[:artist_name]
  ay = data[:album_year]
  at = data[:album_title]
  ti = data[:track_index]
  tt = data[:track_title]

  ay = nil if ay == "0"

  case
  when an && ay && at && ti && tt
    components = [[an], [ay, at], [ti, tt]]
  when an && ay && at && tt
    components = [[an], [ay, at], [tt]]
  when an && at && ti && tt
    components = [[an], [at], [ti, tt]]
  when an && at && tt
    components = [[an], [at], [tt]]
  when an && tt
    components = [[an], [tt]]
  when tt
    components = [[tt]]
  else raise NotEnoughData
  end

  return components.map { |x| x.map { |x| escape_component(x) } }
end

COLUMNS = ENV["COLUMNS"] || 80

def singleton (&body)
  object = Object.new
  object.extend(Module.new(&body))
  object.send :initialize
  return object
end

status_line = singleton do
  attr_reader :width
  def initialize
    @width = 0
  end

  def remaining_width
    COLUMNS - @width
  end
  
  def clear
    print "\b" * COLUMNS
    print " " * COLUMNS
    print "\b" * COLUMNS
    @width = 0
  end

  def update
    clear ; yield ; flush
  end

  def flush
    $stdout.flush
  end
  
  def << string
    count = [remaining_width, string.size].min
    print string[0 ... count]
    @width += count
  end
end

n_total_files = 0
print "Counting files..." ; $stdout.flush
Find.find(*ARGV) { |x| n_total_files += 1 if FileTest.file? x }
puts " #{n_total_files}."

def warn_skip (file_name, message)
  puts "Warning: Skipping file `#{file_name}': #{message}"
end

n_completed_files = 0           # This counts all files.
n_processed_files = 0           # This only counts recognized files.
n_created_links = 0
Find.find *ARGV do |file_name|
  if FileTest.directory? file_name
    status_line.update do
      percent_done = n_completed_files * 100.0 / n_total_files
      status_line << "[%.2f%%] " % percent_done
      count = status_line.remaining_width - "Processing `'...".size
      if file_name.size > count
        file_name_tail = "[...]" + file_name[-count + 5 .. -1]
      else
        file_name_tail = file_name
      end
      status_line << "Processing `#{file_name_tail}'..."
    end
  elsif FileTest.file? file_name
    next if [".jpg", ".jpeg", ".png", ".gif"].include? \
      File.extname(file_name).downcase
    begin

      TagLib::FileRef.open(file_name) do |file|
        n_processed_files += 1
        unless file.null?
          tag = file.tag
          data = { :artist_name => tag.artist,
                   :album_year  => tag.year.to_s,
                   :album_title => tag.album,
                   :track_index => "#{0 if tag.track < 10}#{tag.track}",
                   :track_title => tag.title }

          for key, value in data do
            if (value.nil? or value.blank?)
              data.delete key
            else
              data[key] = value.trim
            end
          end

          components = parse_data(data).map { |x| join_components(*x) }.
                         inject([]) { |a, x| a << join_components(a.last, x) }

          dir_name = components[0...-1] * "/"
          new_file_name = components * "/" + File.extname(file_name)

          FileUtils.mkdir_p(dir_name) if components.length > 1

          begin
            if $hardlinks
              FileUtils.ln(file_name, new_file_name)
            else
              FileUtils.ln_s(file_name, new_file_name)
            end
            n_created_links += 1
          rescue Errno::EEXIST
            if $hardlinks
              raise unless File.stat(file_name).ino ==
                           File.stat(new_file_name).ino
            else
              raise unless FileTest.symlink? new_file_name and
                File.readlink(new_file_name) == file_name
            end
          end
        end
      end

    rescue NotEnoughData
      puts ; warn_skip file_name, "Not enough track data " +
        "(need at least the track title)."
    rescue Errno::EEXIST
      puts ; warn_skip file_name,
        "Cannot create #{$hardlinks ? "hardlink" : "symbolic link"}: " +
        "Conflicting file already exists. [original error: #$!]"
    rescue Interrupt
      puts ; puts "Interrupted." ; exit(1)
    rescue Exception
      puts ; raise
    end

    n_completed_files += 1
  end
end

status_line.update do
  status_line << "[100%] Processing `#{ARGV.last}'..."
end

puts ; puts "Processed #{n_processed_files} media files " +
  "(created #{n_created_links} " +
  "#{$hardlinks ? "hardlinks" : "symbolic links"})."
