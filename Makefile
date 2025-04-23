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
	./bin/ft_ality grammars/valid/mk9.gmr

test:re
	@echo "error/0.gmr :"
	@-./bin/ft_ality grammars/error/0.gmr
	@echo "error/1.gmr :"
	@-./bin/ft_ality grammars/error/1.gmr
	@echo "error/2.gmr :"
	@-./bin/ft_ality grammars/error/2.gmr
	@echo "error/duplicate.gmr :"
	@-./bin/ft_ality grammars/error/duplicate.gmr
	@echo "error/empty.gmr :"
	@-./bin/ft_ality grammars/error/empty.gmr
	@echo "valid/big_mk9.gmr :"
	@./bin/ft_ality grammars/valid/big_mk9.gmr

.PHONY:all clean fclean re run test
