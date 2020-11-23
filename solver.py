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

    def isSolved(self, board):

        for row in board:
            present = [False for k in range(len(board))]
            for n in row:
                if n != ".":
                    present[int(n)-1] = True
                else:
                    return False
            if not all(present):
                return False

        for i in range(len(board)):
            present = [False for k in range(len(board))]
            for j in range(len(board)):
                if board[j][i] != ".":
                    present[int(board[j][i])-1] = True
                else:
                    return False
            if not all(present):
                return False

        for i in range(int(pow(len(board), 0.5))):
            for j in range(int(pow(len(board), 0.5))):
                present = [False for k in range(len(board))]
                for i1 in range(int(pow(len(board), 0.5))):
                    for j1 in range(int(pow(len(board), 0.5))):
                        if board[3*i+i1][3*j+j1] != ".":
                            present[int(board[3*i+i1][3*j+j1]) - 1] = True
                        else:
                            return False
                if not all(present):
                    return False

        return True

    def getPossibleValues(self, board, i, j):

        possibleValues = set([str(k) for k in range(1, len(board)+1)])

        s = set(board[i])
        s = s.union(set([board[k][j] for k in range(len(board))]))
        s2 = set()

        iStart = (i//3) * 3
        jStart = (j//3) * 3

        for i1 in range(iStart, iStart+3):
            for j1 in range(jStart, jStart+3):
                s2.add(board[i1][j1])

        s = s.union(s2)

        return possibleValues.difference(s)
