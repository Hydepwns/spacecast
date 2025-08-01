defmodule Spacecast.Integration.Adapters.ShopifyAdapter do
  @moduledoc """
  Integration adapter for Shopify.

  This adapter provides integration with the Shopify API for e-commerce operations.
  It handles authentication, data mapping, and API communication with Shopify.
  """

  @behaviour Spacecast.Integration.IntegrationAdapter

  require Logger

  # API endpoints
  @shopify_api_version "2023-10"
  @products_endpoint "/admin/api/%{api_version}/products.json"
  @orders_endpoint "/admin/api/%{api_version}/orders.json"

  @impl true
  def validate_config(config) do
    required_keys = [:shop_url, :api_key, :api_secret]

    missing_keys = Enum.filter(required_keys, fn key -> !Map.has_key?(config, key) end)

    if Enum.empty?(missing_keys) do
      # All required keys are present
      :ok
    else
      # Some required keys are missing
      {:error, "Missing required configuration: #{Enum.join(missing_keys, ", ")}"}
    end
  end

  @impl true
  def fetch_data(options) do
    # Get resource type from options
    resource_type = Map.get(options, :resource_type, "product")

    # Determine which endpoint to use
    endpoint = get_endpoint_for_resource_type(resource_type)

    # Get API credentials from options
    config = Map.get(options, :config, %{})

    # Build request URL
    url = build_url(config.shop_url, endpoint, @shopify_api_version)

    # Add query parameters
    url = add_query_params(url, options)

    # Make the API request
    headers = [
      {"X-Shopify-Access-Token", config.api_key},
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ]

    case HTTPoison.get(url, headers) do
      {:ok, %{status_code: 200, body: body}} ->
        # Parse JSON response
        case Jason.decode(body) do
          {:ok, response} ->
            # Extract data based on resource type
            data = extract_data(response, resource_type)
            {:ok, data}

          {:error, reason} ->
            {:error, {:json_parse_error, reason}}
        end

      {:ok, %{status_code: status_code, body: body}} ->
        # Handle error response
        {:error, {:api_error, status_code, body}}

      {:error, reason} ->
        # Handle HTTP client error
        {:error, {:http_error, reason}}
    end
  end

  @impl true
  def export_data(data, options) do
    # Get resource type from options
    resource_type = Map.get(options, :resource_type, "product")

    # Determine which endpoint to use
    endpoint = get_endpoint_for_resource_type(resource_type)

    # Get API credentials from options
    config = Map.get(options, :config, %{})

    # Build request URL
    url = build_url(config.shop_url, endpoint, @shopify_api_version)

    # Transform data to Shopify format
    shopify_data = transform_to_shopify_format(data, resource_type)

    # Make the API request
    headers = [
      {"X-Shopify-Access-Token", config.api_key},
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ]

    # Create request body
    body = Jason.encode!(shopify_data)

    case HTTPoison.post(url, body, headers) do
      {:ok, %{status_code: status_code, body: response_body}} when status_code in 200..201 ->
        # Parse JSON response
        case Jason.decode(response_body) do
          {:ok, response} ->
            # Extract IDs from response
            ids = extract_ids(response, resource_type)
            {:ok, ids}

          {:error, reason} ->
            {:error, {:json_parse_error, reason}}
        end

      {:ok, %{status_code: status_code, body: body}} ->
        # Handle error response
        {:error, {:api_error, status_code, body}}

      {:error, reason} ->
        # Handle HTTP client error
        {:error, {:http_error, reason}}
    end
  end

  @impl true
  def test_connection(config) do
    # Build request URL for a simple API call
    url =
      build_url(
        config.shop_url,
        "/admin/api/#{@shopify_api_version}/shop.json",
        @shopify_api_version
      )

    # Make the API request
    headers = [
      {"X-Shopify-Access-Token", config.api_key},
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ]

    case HTTPoison.get(url, headers) do
      {:ok, %{status_code: 200, body: body}} ->
        # Parse JSON response
        case Jason.decode(body) do
          {:ok, response} ->
            {:ok, response}

          {:error, reason} ->
            {:error, {:json_parse_error, reason}}
        end

      {:ok, %{status_code: status_code, body: body}} ->
        # Handle error response
        {:error, {:api_error, status_code, body}}

      {:error, reason} ->
        # Handle HTTP client error
        {:error, {:http_error, reason}}
    end
  end

  @impl true
  def get_mapping_schema do
    %{
      "product" => %{
        # Shopify field => Resource field
        "id" => :external_id,
        "title" => :name,
        "body_html" => :description,
        "vendor" => :vendor,
        "product_type" => :category,
        "created_at" => :created_at,
        "updated_at" => :updated_at,
        "published_at" => :published_at,
        "variants" => :variants
      },
      "order" => %{
        # Shopify field => Resource field
        "id" => :external_id,
        "order_number" => :order_number,
        "email" => :customer_email,
        "created_at" => :created_at,
        "updated_at" => :updated_at,
        "processed_at" => :processed_at,
        "customer" => :customer,
        "line_items" => :items,
        "shipping_address" => :shipping_address,
        "billing_address" => :billing_address,
        "financial_status" => :payment_status,
        "fulfillment_status" => :fulfillment_status,
        "total_price" => :total_amount
      }
    }
  end

  # Private helper functions

  defp get_endpoint_for_resource_type(resource_type) do
    case resource_type do
      "product" -> @products_endpoint
      "order" -> @orders_endpoint
      _ -> raise "Unsupported resource type: #{resource_type}"
    end
  end

  defp build_url(shop_url, endpoint, api_version) do
    endpoint = String.replace(endpoint, "%{api_version}", api_version)
    URI.merge(shop_url, endpoint) |> URI.to_string()
  end

  defp add_query_params(url, options) do
    # Extract query parameters from options
    limit = Map.get(options, :limit, 50)
    page = Map.get(options, :page, 1)

    # Build query string
    query_params = %{
      limit: limit,
      page: page
    }

    # Add filters if present
    query_params =
      if Map.has_key?(options, :filters) do
        Map.merge(query_params, options.filters)
      else
        query_params
      end

    # Add query string to URL
    uri = URI.parse(url)
    query = URI.encode_query(query_params)

    %{uri | query: query} |> URI.to_string()
  end

  defp extract_data(response, resource_type) do
    # Extract data from response based on resource type
    case resource_type do
      "product" ->
        # Products are nested under "products" key
        products = Map.get(response, "products", [])

        # Map to our internal format
        Enum.map(products, &map_product/1)

      "order" ->
        # Orders are nested under "orders" key
        orders = Map.get(response, "orders", [])

        # Map to our internal format
        Enum.map(orders, &map_order/1)

      _ ->
        []
    end
  end

  defp extract_ids(response, resource_type) do
    # Extract IDs from response based on resource type
    case resource_type do
      "product" ->
        # Product is nested under "product" key for single product creation
        product = Map.get(response, "product", %{})
        [Map.get(product, "id")]

      "order" ->
        # Order is nested under "order" key for single order creation
        order = Map.get(response, "order", %{})
        [Map.get(order, "id")]

      _ ->
        []
    end
  end

  defp transform_to_shopify_format(data, resource_type) do
    # Transform data to Shopify format based on resource type
    case resource_type do
      "product" ->
        # Wrap in "product" key
        %{"product" => Enum.map(data, &map_to_shopify_product/1)}

      "order" ->
        # Wrap in "order" key
        %{"order" => Enum.map(data, &map_to_shopify_order/1)}

      _ ->
        %{}
    end
  end

  defp map_product(shopify_product) do
    # Map Shopify product to our internal format
    %{
      id: to_string(shopify_product["id"]),
      name: shopify_product["title"],
      description: shopify_product["body_html"],
      vendor: shopify_product["vendor"],
      category: shopify_product["product_type"],
      created_at: parse_datetime(shopify_product["created_at"]),
      updated_at: parse_datetime(shopify_product["updated_at"]),
      published_at: parse_datetime(shopify_product["published_at"]),
      variants: map_variants(shopify_product["variants"] || []),
      external_id: to_string(shopify_product["id"]),
      external_source: "shopify"
    }
  end

  defp map_order(shopify_order) do
    # Map Shopify order to our internal format
    %{
      id: to_string(shopify_order["id"]),
      order_number: shopify_order["order_number"],
      customer_email: shopify_order["email"],
      created_at: parse_datetime(shopify_order["created_at"]),
      updated_at: parse_datetime(shopify_order["updated_at"]),
      processed_at: parse_datetime(shopify_order["processed_at"]),
      customer: map_customer(shopify_order["customer"]),
      items: map_line_items(shopify_order["line_items"] || []),
      shipping_address: map_address(shopify_order["shipping_address"]),
      billing_address: map_address(shopify_order["billing_address"]),
      payment_status: shopify_order["financial_status"],
      fulfillment_status: shopify_order["fulfillment_status"],
      total_amount: parse_decimal(shopify_order["total_price"]),
      external_id: to_string(shopify_order["id"]),
      external_source: "shopify"
    }
  end

  defp map_to_shopify_product(product) do
    # Map our internal product to Shopify format
    %{
      "title" => product.name,
      "body_html" => product.description,
      "vendor" => product.vendor,
      "product_type" => product.category,
      "published" => true,
      "variants" => map_to_shopify_variants(product.variants || [])
    }
  end

  defp map_to_shopify_order(order) do
    # Map our internal order to Shopify format
    %{
      "email" => order.customer_email,
      "line_items" => map_to_shopify_line_items(order.items || []),
      "shipping_address" => map_to_shopify_address(order.shipping_address),
      "billing_address" => map_to_shopify_address(order.billing_address)
    }
  end

  defp map_variants(shopify_variants) do
    Enum.map(shopify_variants, fn variant ->
      %{
        id: to_string(variant["id"]),
        sku: variant["sku"],
        price: parse_decimal(variant["price"]),
        inventory_quantity: variant["inventory_quantity"],
        external_id: to_string(variant["id"])
      }
    end)
  end

  defp map_line_items(shopify_line_items) do
    Enum.map(shopify_line_items, fn item ->
      %{
        product_id: to_string(item["product_id"]),
        variant_id: to_string(item["variant_id"]),
        quantity: item["quantity"],
        unit_price: parse_decimal(item["price"]),
        total_price: parse_decimal(item["price"]) |> Decimal.mult(Decimal.new(item["quantity"])),
        external_id: to_string(item["id"])
      }
    end)
  end

  defp map_customer(shopify_customer) do
    if shopify_customer do
      %{
        id: to_string(shopify_customer["id"]),
        email: shopify_customer["email"],
        first_name: shopify_customer["first_name"],
        last_name: shopify_customer["last_name"],
        external_id: to_string(shopify_customer["id"])
      }
    else
      nil
    end
  end

  defp map_address(shopify_address) do
    if shopify_address do
      %{
        first_name: shopify_address["first_name"],
        last_name: shopify_address["last_name"],
        address1: shopify_address["address1"],
        address2: shopify_address["address2"],
        city: shopify_address["city"],
        province: shopify_address["province"],
        country: shopify_address["country"],
        zip: shopify_address["zip"],
        phone: shopify_address["phone"]
      }
    else
      nil
    end
  end

  defp map_to_shopify_variants(variants) do
    Enum.map(variants, fn variant ->
      %{
        "sku" => variant.sku,
        "price" => to_string(variant.price),
        "inventory_quantity" => variant.inventory_quantity
      }
    end)
  end

  defp map_to_shopify_line_items(items) do
    Enum.map(items, fn item ->
      %{
        "variant_id" => item.variant_id,
        "quantity" => item.quantity
      }
    end)
  end

  defp map_to_shopify_address(address) do
    if address do
      %{
        "first_name" => address.first_name,
        "last_name" => address.last_name,
        "address1" => address.address1,
        "address2" => address.address2,
        "city" => address.city,
        "province" => address.province,
        "country" => address.country,
        "zip" => address.zip,
        "phone" => address.phone
      }
    else
      nil
    end
  end

  defp parse_datetime(nil), do: nil

  defp parse_datetime(datetime_string) do
    case DateTime.from_iso8601(datetime_string) do
      {:ok, datetime, _} -> datetime
      _ -> nil
    end
  end

  defp parse_decimal(nil), do: Decimal.new("0")

  defp parse_decimal(price_string) when is_binary(price_string) do
    Decimal.new(price_string)
  end

  defp parse_decimal(price) when is_number(price) do
    Decimal.new(to_string(price))
  end
end
