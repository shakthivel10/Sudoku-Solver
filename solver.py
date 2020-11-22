class SudokuSolver:

    def solveSudoku(self, board):

        if self.isSolved(board):
            return

        i1, j1 = 0, 0

        for i in range(len(board)):
            for j in range(len(board[0])):
                if board[i][j] == ".":
                    i1, j1 = i, j
                    break
            else:
                continue
            break

        possibleValues = self.getPossibleValues(board, i1, j1)

        if possibleValues is None:
            return

        for v in possibleValues:
            board[i][j] = v
            self.solveSudoku(board)

            if self.isSolved(board):
                return

        board[i][j] = "."
