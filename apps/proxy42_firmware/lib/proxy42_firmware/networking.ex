defmodule Proxy42.Firmware.Networking do
  # Refer https://github.com/nerves-project/nerves_network
  alias Nerves.Network

  def start(_type \\ :normal, _args \\ []) do
    # Network.setup "wlan0", ssid: "accesspoint name", key_mgmt: :"WPA-PSK", psk: "password"
    Network.setup "eth0", ipv4_address_method: :dhcp
  end
end
