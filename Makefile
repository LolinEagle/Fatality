all:
	@dune build @install
	@dune install --prefix .
	@echo "Building done"

clean:
	@dune clean
	@rm -rf _build lib doc automaton
	@echo "Cleaning done"

fclean:clean
	@rm -rf bin

re:fclean
	@make all

run:re
	./bin/ft_ality grammars/mk9.gmr

.PHONY:all clean fclean re run
