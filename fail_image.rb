#!/usr/bin/env ruby

require "rexml/document"
require "base64"

f = "cts_build/external/openglcts/modules/TestResults.qpa"

s = File::read(f)

cases = s.scan(/#beginTestCaseResult \S+(.+?)#endTestCaseResult/m)

cases.each do |c|
  doc = REXML::Document.new(c[0])

  REXML::XPath.each(doc, "//ImageSet") do |imageset|
    if imageset.elements.size == 3

      imageset.elements.each do |image|
        name = image.attributes.fetch("Name")

        body = Base64.decode64(image.text)

        File::write("fail-#{name}.png", body)
      end

      puts "Found images!"
      exit
    end
  end
end
