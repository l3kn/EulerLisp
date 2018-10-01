def build_application_chain(chars, initial_body)
  chars.reverse.reduce(initial_body) do |body, c|
    build_application(c, body)
  end
end

def build_application(char, body)
  "(#{char}st #{body})"
end

(2..4).each do |i|
  (0...(2**i)).each do |n|
    name = n.to_s(2).rjust(i, "0").tr("01", "fr")
    puts "(defn #{name}st (lst) #{build_application_chain(name.chars, 'lst')})"
  end
end

