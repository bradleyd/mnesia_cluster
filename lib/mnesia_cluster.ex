defmodule MnesiaCluster do
  require Logger
  @moduledoc """
  Documentation for MnesiaCluster.
  """

  @table Person

  def start do
    # does table exist
      :mnesia.start
      :mnesia.subscribe(:system)
      :mnesia.subscribe(:activity)
      spawn(fn -> __MODULE__.listen end)
      case :mnesia.system_info(:tables) |> Enum.find(fn(x) -> x == @table end) do
        nil ->
          # am i the first node
          case Node.list do
            [] ->
              # first node in cluster
              :mnesia.stop
              :mnesia.create_schema([node()])
              :mnesia.start
              :mnesia.create_table(Person, [attributes: [:id, :name, :job]])
            nodes ->
              # already a mnesia cluster, rpc call
              case :rpc.call(List.first(nodes), :mnesia, :change_config, [:extra_db_nodes, [node()]]) do
                ok ->
                  :mnesia.add_table_copy(@table, node(), :ram_copies)
                :error -> :error
              end
          end
        @table ->
          :ok
      end
  end

  def listen do
    receive do
      {:mnesia_system_event, message} -> Logger.warn(inspect(message))
      {:mnesia_activity_event, activity_message} -> Logger.warn(inspect(activity_message))
      not_matched -> IO.inspect not_matched
    end
    listen
  end

  defp non_schema_table_exist([], acc), do: acc
  defp non_schema_table_exist([:schema|t], acc) do
    non_schema_table_exist(t, acc)
  end
  defp non_schema_table_exist([h|t], acc) do
    non_schema_table_exist(t, [acc|h])
  end


end
