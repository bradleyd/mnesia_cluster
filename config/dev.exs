use Mix.Config

config :mnesia_cluster, table: Person
config :libcluster,
topologies: [
  mnesia_nodes: [
    strategy: MnesiaCluster.Cluster,
    config: [
      port: 45892,
      if_addr: {0,0,0,0},
      multicast_addr: {230,1,1,251},
      # a TTL of 1 remains on the local network,
      # use this to change the number of jumps the
      # multicast packets will make
      multicast_ttl: 1]]]
