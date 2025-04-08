all:
	@dune build @install
	@dune install --prefix .
	@echo "Building done"

clean:
	@dune clean
	@rm -rf _build lib doc
	@echo "Cleaning done"

fclean:clean
	@rm -rf bin

re:clean all

.PHONY:all clean fclean re
