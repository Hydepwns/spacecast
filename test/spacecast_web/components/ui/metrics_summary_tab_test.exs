defmodule SpacecastWeb.Components.UI.MetricsSummaryTabTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.UI.MetricsSummaryTab

  describe "MetricsSummaryTab component" do
    test "renders summary with metrics data" do
      metrics = %{
        execution_count: 150,
        average_execution_time_ms: 45.5,
        success_rate: 0.92,
        error_rate: 0.08,
        success_count: 138,
        error_count: 12
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "150"
      assert html =~ "45.5ms"
      assert html =~ "92.0%"
      assert html =~ "12"
      assert html =~ "Total Executions"
      assert html =~ "Avg Execution Time"
      assert html =~ "Success Rate"
      assert html =~ "Errors"
    end

    test "renders summary with zero executions" do
      metrics = %{
        execution_count: 0,
        average_execution_time_ms: 0.0,
        success_rate: 0.0,
        error_rate: 0.0,
        success_count: 0,
        error_count: 0
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "0"
      assert html =~ "N/A"
      assert html =~ "No data available"
    end

    test "renders summary with high success rate" do
      metrics = %{
        execution_count: 1000,
        average_execution_time_ms: 12.3,
        success_rate: 0.99,
        error_rate: 0.01,
        success_count: 990,
        error_count: 10
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "1000"
      assert html =~ "12.3ms"
      assert html =~ "99.0%"
      assert html =~ "1.0%"
      assert html =~ "990"
      assert html =~ "10"
    end

    test "renders summary with high error rate" do
      metrics = %{
        execution_count: 50,
        average_execution_time_ms: 200.0,
        success_rate: 0.2,
        error_rate: 0.8,
        success_count: 10,
        error_count: 40
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "50"
      assert html =~ "200.0ms"
      assert html =~ "20.0%"
      assert html =~ "80.0%"
      assert html =~ "10"
      assert html =~ "40"
    end

    test "renders pie chart with success rate" do
      metrics = %{
        execution_count: 100,
        average_execution_time_ms: 25.0,
        success_rate: 0.75,
        error_rate: 0.25,
        success_count: 75,
        error_count: 25
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "75%"
      assert html =~ "Success"
      assert html =~ "Error"
      assert html =~ "75 (75.0%)"
      assert html =~ "25 (25.0%)"
    end

    test "handles custom formatting functions" do
      metrics = %{
        execution_count: 42,
        average_execution_time_ms: 123.456,
        success_rate: 0.857,
        error_rate: 0.143,
        success_count: 36,
        error_count: 6
      }

      format_time = fn time -> "~#{Float.round(time, 0)}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 0)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "42"
      assert html =~ "~123ms"
      assert html =~ "86%"
      assert html =~ "14%"
    end

    test "handles edge case with single execution" do
      metrics = %{
        execution_count: 1,
        average_execution_time_ms: 100.0,
        success_rate: 1.0,
        error_rate: 0.0,
        success_count: 1,
        error_count: 0
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 1)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "1"
      assert html =~ "100.0ms"
      assert html =~ "100.0%"
      assert html =~ "0.0%"
    end

    test "handles very large numbers" do
      metrics = %{
        execution_count: 1_000_000,
        average_execution_time_ms: 0.001,
        success_rate: 0.9999,
        error_rate: 0.0001,
        success_count: 999_900,
        error_count: 100
      }

      format_time = fn time -> "#{time}ms" end
      format_percentage = fn rate -> "#{Float.round(rate * 100, 2)}%" end

      html =
        MetricsSummaryTab.render_summary_tab(%{
          metrics: metrics,
          format_time: format_time,
          format_percentage: format_percentage
        })

      assert html =~ "1000000"
      assert html =~ "0.001ms"
      assert html =~ "99.99%"
      assert html =~ "0.01%"
      assert html =~ "999900"
      assert html =~ "100"
    end
  end
end
