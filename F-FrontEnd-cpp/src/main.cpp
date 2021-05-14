extern "C"
{
int execute_args(int argc, char *argv[]);
}

int main(int argc, char *argv[])
{
    return execute_args(argc, argv);;
}
