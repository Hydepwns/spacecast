defmodule SpacecastWeb.Components.UI.MetricsTableTabTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.UI.MetricsTableTab

  describe "MetricsTableTab component" do
    test "renders table with metrics data" do
      data = %{
        "user_validation" => %{
          execution_count: 100,
          total_execution_time_ms: 5000,
          success_count: 95,
          error_count: 5
        },
        "email_processing" => %{
          execution_count: 50,
          total_execution_time_ms: 2500,
          success_count: 48,
          error_count: 2
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> String.replace(name, "_", " ") |> String.upcase() end
      sort_function = fn data -> Enum.sort(data, fn {a, _}, {b, _} -> a <= b end) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Transformation Performance",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No transformation data available"
      })

      assert html =~ "Transformation Performance"
      assert html =~ "USER VALIDATION"
      assert html =~ "EMAIL PROCESSING"
      assert html =~ "100"
      assert html =~ "50"
      assert html =~ "50.0ms"
      assert html =~ "50.0ms"
      assert html =~ "95.0%"
      assert html =~ "96.0%"
      assert html =~ "5"
      assert html =~ "2"
    end

    test "renders table with empty data" do
      data = %{}

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Test Performance",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No test data available"
      })

      assert html =~ "Test Performance"
      assert html =~ "No test data available"
      refute html =~ "Name"
      refute html =~ "Count"
    end

    test "renders table with single item" do
      data = %{
        "single_test" => %{
          execution_count: 1,
          total_execution_time_ms: 100,
          success_count: 1,
          error_count: 0
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> String.upcase(name) end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Single Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "Single Test"
      assert html =~ "SINGLE TEST"
      assert html =~ "1"
      assert html =~ "100.0ms"
      assert html =~ "100.0%"
      assert html =~ "0"
    end

    test "handles zero execution count" do
      data = %{
        "zero_test" => %{
          execution_count: 0,
          total_execution_time_ms: 0,
          success_count: 0,
          error_count: 0
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Zero Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "0"
      assert html =~ "0.0ms"
      assert html =~ "NaN%"
      assert html =~ "0"
    end

    test "handles custom formatting functions" do
      data = %{
        "custom_test" => %{
          execution_count: 42,
          total_execution_time_ms: 1234,
          success_count: 40,
          error_count: 2
        }
      }

      format_time = fn time -> "~#{Float.round(time, 0)}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 0)}%" end
      format_name = fn name -> "CUSTOM: #{name}" end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Custom Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "42"
      assert html =~ "~29ms"
      assert html =~ "95%"
      assert html =~ "CUSTOM: custom_test"
    end

    test "handles sorting function" do
      data = %{
        "zebra" => %{execution_count: 10, total_execution_time_ms: 100, success_count: 8, error_count: 2},
        "alpha" => %{execution_count: 5, total_execution_time_ms: 50, success_count: 5, error_count: 0},
        "beta" => %{execution_count: 15, total_execution_time_ms: 150, success_count: 12, error_count: 3}
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data, fn {a, _}, {b, _} -> a <= b end) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Sorting Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      # Check that items are sorted alphabetically
      alpha_index = String.indexOf(html, "alpha")
      beta_index = String.indexOf(html, "beta")
      zebra_index = String.indexOf(html, "zebra")

      assert alpha_index < beta_index
      assert beta_index < zebra_index
    end

    test "handles very large numbers" do
      data = %{
        "large_test" => %{
          execution_count: 1_000_000,
          total_execution_time_ms: 50_000_000,
          success_count: 999_500,
          error_count: 500
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 2)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Large Numbers Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "1000000"
      assert html =~ "50.0ms"
      assert html =~ "99.95%"
      assert html =~ "500"
    end

    test "handles edge case with 100% success rate" do
      data = %{
        "perfect_test" => %{
          execution_count: 100,
          total_execution_time_ms: 1000,
          success_count: 100,
          error_count: 0
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Perfect Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "100"
      assert html =~ "10.0ms"
      assert html =~ "100.0%"
      assert html =~ "0"
    end

    test "handles edge case with 100% error rate" do
      data = %{
        "failed_test" => %{
          execution_count: 10,
          total_execution_time_ms: 500,
          success_count: 0,
          error_count: 10
        }
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end
      format_name = fn name -> name end
      sort_function = fn data -> Enum.sort(data) end

      html = MetricsTableTab.render_metrics_table_tab(%{
        title: "Failed Test",
        data: data,
        format_time: format_time,
        format_percentage: format_percentage,
        format_name: format_name,
        sort_function: sort_function,
        no_data_message: "No data"
      })

      assert html =~ "10"
      assert html =~ "50.0ms"
      assert html =~ "0.0%"
      assert html =~ "10"
    end
  end
end
