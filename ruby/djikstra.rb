#encoding: UTF-8
=begin
while true
    print "1-grafi bastirmak icin ==>1\n"
    print "2-graftaki noktalar arasi uzakliga bakmak icin uzaklik==>2\n"
    print "3-graftaki x noktanin komsularini gorebilmek icin ==>3\n"
    print "4-graftaki 2 nokta arasindaki en kisa mesafeyi gormek icin ==>4\n"
    secenek = gets.chomp
    print "\n"
    if secenek=="1"
      p graph
    elsif secenek=="2"
      print "ilk nokta==>"
      x=gets.chomp
      print "ikinci nokta==>"
      y=gets.chomp
      p graph.length_between(Integer(x),Integer(y))
    elsif secenek=="3"
      print "x==>?"
      x=gets.chomp
      p graph.neighbors(Integer(x))
    elsif secenek=="4"
      print "ilk nokta==>"
      x=gets.chomp
      print "ikinci nokta==>"
      y=gets.chomp
      p graph.dijkstra(Integer(x),Integer(y))
    end
    print "\n"
end
=end

=begin
Öncelikle selamün aleyküm arkadaşlar ben dj dikkat,
daha güzelini getirin göreyim. hadi eyw.
=end

Edge = Struct.new(:src, :dst, :length)
class Graph < Array
  attr_reader :edges
  
  def initialize
    @edges = []
  end
  def length_between(src, dst)
    @edges.each do |edge|
      return edge.length if edge.src == src and edge.dst == dst
    end
    nil
  end
  def neighbors(vertex)
    neighbors = []
    @edges.each do |edge|
      neighbors.push edge.dst if edge.src == vertex
    end
    return neighbors.uniq
  end

  def connect(src, dst, length = 1)
    if !(self.include?(src))
      raise ArgumentException, "No such vertex: #{src}"
    end
    if !(self.include?(dst))
      raise ArgumentException, "No such vertex: #{dst}"
    end
    @edges.push Edge.new(src, dst, length)
  end
  
  def connect_mutually(vertex1, vertex2, length = 1)
    self.connect vertex1, vertex2, length
    self.connect vertex2, vertex1, length
  end


  def dijkstra(src, dst = nil)
    distances = {}
    previouses = {}
    self.each do |vertex|
      distances[vertex] = nil # Infinity
      previouses[vertex] = nil
    end
    distances[src] = 0
    vertices = self.clone
    until vertices.empty?
      nearest_vertex = vertices.inject do |a, b|
        next b unless distances[a]
        next a unless distances[b]
        next a if distances[a] < distances[b]
        b
      end
      break unless distances[nearest_vertex] # Infinity
      if dst and nearest_vertex == dst
        return distances[dst]
      end
      neighbors = vertices.neighbors(nearest_vertex)
      neighbors.each do |vertex|
        alt = distances[nearest_vertex] + vertices.length_between(nearest_vertex, vertex)
        if distances[vertex].nil? or alt < distances[vertex]
          distances[vertex] = alt
          previouses[vertices] = nearest_vertex
          # decrease-key v in Q # ???
        end
      end
      vertices.delete nearest_vertex
    end
    if dst
      return nil
    else
      return distances
    end
  end
end

#graphı inceleyiniz sayın hocam, yanlış nokta girmeyiniz lütfen
graph = Graph.new
(1..6).each {|node| graph.push node }
graph.connect_mutually 1, 2, 7
graph.connect_mutually 1, 3, 9
graph.connect_mutually 1, 6, 14
graph.connect_mutually 2, 3, 10
graph.connect_mutually 2, 4, 15
graph.connect_mutually 3, 4, 11
graph.connect_mutually 3, 6, 2
graph.connect_mutually 4, 5, 6
graph.connect_mutually 5, 6, 9
#graph ending main is starting
while true
    print "1-grafi bastirmak icin ==>1\n"
    print "2-graftaki noktalar arasi uzakliga bakmak icin uzaklik==>2\n"
    print "3-graftaki x noktanin komsularini gorebilmek icin ==>3\n"
    print "4-graftaki 2 nokta arasindaki en kisa mesafeyi gormek icin ==>4\n"
    secenek = gets.chomp
    print "\n"
    if secenek=="1"
      p graph
    elsif secenek=="2"
      print "ilk nokta==>"
      x=gets.chomp
      print "ikinci nokta==>"
      y=gets.chomp
      p graph.length_between(Integer(x),Integer(y))
    elsif secenek=="3"
      print "x==>?"
      x=gets.chomp
      p graph.neighbors(Integer(x))
    elsif secenek=="4"
      print "ilk nokta==>"
      x=gets.chomp
      print "ikinci nokta==>"
      y=gets.chomp
      p graph.dijkstra(Integer(x),Integer(y))
    end
    print "\n"
end
