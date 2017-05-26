defmodule MnesiaCluster.Setup do
  use GenServer
  require Logger

  @table Application.get_env(:mnesia_cluster, :table)

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(state \\ []) do
    :mnesia.start
    :erlang.send_after(1000, __MODULE__, :subscribe)
    spawn_link(fn -> start() end)
    {:ok, state}
  end

  def start do
    # does table exist
    case does_table_already_exist(tables(), nil) do
      nil -> create_or_attach_to_cluster(Node.list)
      table -> :mnesia.wait_for_tables([table], 5000)
    end
  end

  # Private functions
  def handle_call(_message, _from, state) do
    {:reply, :ok, state}
  end
  def handle_cast(_message, state) do
    {:noreply, state}
  end

  def handle_info(:subscribe, state) do
    :mnesia.subscribe(:system)
    :mnesia.subscribe(:activity)
    {:noreply, state}
  end
  # TODO take action on events..node down etc http://erlang.org/doc/apps/mnesia/Mnesia_chap5.html#event_handling
  def handle_info({:mnesia_system_event, system_message}, state) do
    Logger.warn("Received mnesia system event: #{inspect(system_message)}")
    {:noreply, state}
  end
  # TODO take action on events..node down etc http://erlang.org/doc/apps/mnesia/Mnesia_chap5.html#event_handling
  def handle_info({:mnesia_activity_event, activity_message}, state) do
    Logger.warn("Received mnesia activity event: #{inspect(activity_message)}")
    {:noreply, state}
  end
  def handle_info(_message, state) do
    {:noreply, state}
  end

  # TODO add table schema to another file like Amnesia or use Amnesia?
  defp create_or_attach_to_cluster([]) do
    # first node in cluster
    :mnesia.stop
    :mnesia.create_schema([node()])
    :mnesia.start
    :mnesia.create_table(@table, [attributes: [:id, :name, :job]])
  end
  defp create_or_attach_to_cluster(nodes) do
    # add this node to cluster
    case :rpc.call(List.first(nodes), :mnesia, :change_config, [:extra_db_nodes, [node()]]) do
      {:ok, result} ->
        Logger.debug("Adding node to cluster results: #{inspect(result)}")
        :mnesia.add_table_copy(@table, node(), :ram_copies)
      {:error, error} -> error
    end
  end

  defp tables() do
    :mnesia.system_info(:tables)
  end

  def does_table_already_exist([], acc), do: acc
  # if we find it, just stop looking
  def does_table_already_exist([@table|_t], _acc), do: @table
  def does_table_already_exist([_h|t], acc) do
    does_table_already_exist(t, acc)
  end

end
