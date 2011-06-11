#!/usr/bin/ruby

require 'fileutils'

DOCTEST="doctest"
FileUtils.mkdir_p(DOCTEST)
begin
  Dir.chdir DOCTEST do |cwd|
    system("doctest ../src/Git.hs --optghc=-i../src")
  end
ensure
  FileUtils.rm_rf(DOCTEST)
end
