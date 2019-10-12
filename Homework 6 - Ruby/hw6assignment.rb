class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)

                rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T

                [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2]]],

                rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L

                rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L

                rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S

                rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z

                rotations([[0, 0], [0,1], [1,1]]), #new 3

                [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], #new 2
                    [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],

                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]])] #new 1

  def self.next_piece(board, cheat_piece)
    if !cheat_piece
      MyPiece.new(All_My_Pieces.sample, board)
    else
      MyPiece.new([[[0,0]]], board)
    end
  end
end

class MyBoard < Board
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self, false)
    @score = 0
    @game = game
    @delay = 500
    @cheat_piece = false
  end


  def next_piece
    @current_block = MyPiece.next_piece(self, @cheat_piece)
    @cheat_piece = false
    @current_pos = nil
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  def check_cheat
    if !game_over? and @game.is_running? and score>100 and !@cheat_piece
        @score -= 100
        @cheat_piece = true
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..((locations.length) - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.check_cheat})
  end
end

