def main():
    # TODO: doesn't work yet
    try:
        report_cmd = f"learn-ocaml grade --exercises={exercise_folder_path} --grade-student={solution_file_path} --timeout 60 --dump-reports {report_name}"                
        process = subprocess.run(report_cmd, shell=True, timeout=30, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if (process.stderr):
            res = process.stderr
            points = re.findall(r"(\d+) points", res)
            if points: 
                updated_record["grade"] = int(points[0])
                print(f"grade found from stderr: {updated_record['grade']}")
            else:
                print(f"Report Output: {res} {record['studentId']} {str(hw_num)} ")
        report_html = report_name + ".report.html"
        report_txt = report_name + ".report.txt"
        if (os.path.exists(report_txt)):
            points = read_points(report_txt)
            if points:
                updated_record["grade"] = points
                print(f"grade found from report: {updated_record['grade']}")
            os.remove(report_html)
            os.remove(report_txt)
        destdb[collection_dest_name].insert_one(updated_record)
    except subprocess.TimeoutExpired:
        print(f"Timeout for file {record['studentId']}")




if __name__ == "__main__":
    main()